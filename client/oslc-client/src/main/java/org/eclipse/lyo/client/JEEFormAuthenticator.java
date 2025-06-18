/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client;

import jakarta.ws.rs.ProcessingException;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.ClientRequestContext;
import jakarta.ws.rs.client.ClientRequestFilter;
import jakarta.ws.rs.client.ClientResponseContext;
import jakarta.ws.rs.client.ClientResponseFilter;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.client.Invocation;
import jakarta.ws.rs.core.Form;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedHashMap;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.core.Response;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A filter that can be registered in order to non-preemptively handle JEE Form
 * based authentication challenges.
 *
 * @author jamsden
 *
 */
public class JEEFormAuthenticator implements ClientRequestFilter, ClientResponseFilter {
    private final Logger log = LoggerFactory.getLogger(JEEFormAuthenticator.class);

    private static final String COOKIE = "Cookie";

    // security params
    private static final String J_SECURITY_CHECK = "j_security_check";
    private static final String J_USERNAME = "j_username";
    private static final String J_PASSWORD = "j_password";
    private static final String FORM_AUTHENTICATOR_REUSED =
            "org.eclipse.lyo.client.oslc.JEEFormAuthenticator.reused";
    private static final String JAZZ_AUTH_MESSAGE_HEADER = "X-com-ibm-team-repository-web-auth-msg";
    private static final String JAZZ_AUTH_REQUIRED = "authrequired";
    private static final String JAZZ_AUTH_FAILED = "authfailed";

    private final String userId;
    private final String password;
    private final String baseUri;
    private boolean followingRedirects = false;
    Client authClient = null;
    private final List<Object> cookies = new ArrayList<>();

    // requires by @Provider
    public JEEFormAuthenticator() {
        this.userId = null;
        this.password = null;
        this.baseUri = null;
    }

    /**
     * @param baseUri base URI for the server, e.g., https://host:9443/ccm
     * @param username user's credentials
     * @param password
     */
    public JEEFormAuthenticator(
            final String baseUri, final String username, final String password) {
        this.userId = username;
        this.password = password;
        this.baseUri = baseUri;
    }

    /**
     * Checks to see if the response is a 401 UNAUTHORIZED. If so, it attempts to
     * authenticate the user, and then retries the request with the updated
     * session information.
     *
     * @see jakarta.ws.rs.client.ClientResponseFilter#filter(jakarta.ws.rs.client.ClientRequestContext, jakarta.ws.rs.client.ClientResponseContext)
     */
    @Override
    public void filter(ClientRequestContext request, ClientResponseContext response) {
        if (followingRedirects) return;

        boolean authRequired =
                JAZZ_AUTH_REQUIRED.equals(response.getHeaderString(JAZZ_AUTH_MESSAGE_HEADER));
        boolean authAlreadyAttempted = isAuthAlreadyAttempted(request);

        if (authRequired && authAlreadyAttempted) {
            response.setStatus(Response.Status.UNAUTHORIZED.getStatusCode());
            log.trace("Jazz auth cookies were appended but the request was UNAUTHORIZED anyway");
            return;
        } else if (!authRequired) {
            log.trace(
                    "Response was non-401, skipping the ClientResponseFilter for Jazz Forms auth");
            return;
        }

        log.trace("Response is 401, attempting Jazz Forms authentication");
        // We got an Authentication challenge, attempt to authenticate the user
        cookies.clear();
        authClient = request.getClient();

        final Form form = new Form();
        form.param(J_USERNAME, this.userId);
        form.param(J_PASSWORD, this.password);
        Response authResponse =
                authClient
                        .target(this.baseUri)
                        .path(J_SECURITY_CHECK)
                        .request(MediaType.APPLICATION_FORM_URLENCODED)
                        .property(FORM_AUTHENTICATOR_REUSED, "true") // only post once
                        .header("Accept", "*/*")
                        .header("X-Requested-With", "XMLHttpRequest")
                        .header(OSLCConstants.OSLC_CORE_VERSION, OSLCConstants.OSLC2_0)
                        .post(Entity.form(form));
        authResponse.getCookies().values().forEach(cookie -> cookies.add(cookie.toCookie()));
        int statusCode = authResponse.getStatus();
        // Check the result
        String jazzAuthMessage = authResponse.getHeaderString(JAZZ_AUTH_MESSAGE_HEADER);

        if (jazzAuthMessage != null && jazzAuthMessage.equalsIgnoreCase(JAZZ_AUTH_FAILED)) {
            authResponse.close();
            response.setStatus(Response.Status.UNAUTHORIZED.getStatusCode());
            log.trace("Jazz Forms authentication failed");
            return;
        }

        String location = authResponse.getHeaderString("Location");
        try {
            authResponse.close();
        } catch (ProcessingException e) {
            log.warn("Connection not closed cleanly");
        }
        statusCode = followRedirects(statusCode, location);

        // retry the request with the updated cookies
        Client requestClient = request.getClient();
        Invocation.Builder retryBuilder =
                requestClient.target(request.getUri()).request(request.getMediaType());
        retryBuilder.property(FORM_AUTHENTICATOR_REUSED, "true"); // prevent infinite loops
        MultivaluedMap<String, Object> newHeaders = new MultivaluedHashMap<>();
        newHeaders.putAll(request.getHeaders());
        newHeaders.add(COOKIE, cookies);
        retryBuilder.headers(newHeaders);
        Invocation invocation;
        String requestMethod = request.getMethod();
        if (request.getEntity() == null) {
            invocation = retryBuilder.build(requestMethod);
        } else {
            invocation =
                    retryBuilder.build(
                            requestMethod,
                            Entity.entity(request.getEntity(), request.getMediaType()));
        }
        log.trace("Retrying the failed request with Jazz cookies");
        Response retryResponse = invocation.invoke();

        if (retryResponse.hasEntity()) {
            response.setEntityStream(retryResponse.readEntity(InputStream.class));
        }
        MultivaluedMap<String, String> headers = response.getHeaders();
        headers.clear();
        headers.putAll(retryResponse.getStringHeaders());
        response.setStatus(retryResponse.getStatus());
    }

    private boolean isAuthAlreadyAttempted(ClientRequestContext request) {
        return "true".equals(request.getProperty(FORM_AUTHENTICATOR_REUSED));
    }

    private int followRedirects(int statusCode, String location) {
        followingRedirects = true;
        while (location != null && statusCode >= 301 && statusCode <= 399) {
            Response lastRedirectResponse = authClient.target(location).request().get();
            statusCode = lastRedirectResponse.getStatus();
            location = lastRedirectResponse.getHeaderString("Location");
            try {
                lastRedirectResponse.close();
            } catch (ProcessingException e) {
                log.warn("Last redirect connection not closed cleanly");
            }
        }
        followingRedirects = false;
        return statusCode;
    }

    @Override
    public void filter(ClientRequestContext requestContext) {
        if (cookies.size() > 0) {
            requestContext.getHeaders().add(COOKIE, cookies);
            log.trace("Jazz auth cookies appended to the request");
        } else {
            log.trace("Not appending Jazz auth cookies to the request");
        }
    }
}
