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
package org.eclipse.lyo.server.oauth.core;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.core.UriBuilder;
import java.io.IOException;
import java.net.URISyntaxException;
import net.oauth.OAuth;
import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.OAuthProblemException;
import net.oauth.OAuthValidator;
import net.oauth.server.OAuthServlet;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Validates that a request is authorized. The request must contain a valid
 * access token and pass {@link OAuthValidator} tests. To change the validator
 * used, call {@link OAuthConfiguration#setValidator(OAuthValidator)}.
 *
 * <p>
 * Usage:
 *
 * <pre>
 * try {
 *     OAuthRequest request = new OAuthRequest(httpRequest);
 *     request.validate();
 * } catch (OAuthException e) {
 *     // Request failed validation. Send an unauthorized response.
 *     OAuthServlet.handleException(httpResponse, e, OAuthConfiguration.getInstance().getRealm());
 * }
 * </pre>
 *
 * @author Samuel Padgett
 */
public class OAuthRequest {
    private static final Logger log = LoggerFactory.getLogger(OAuthRequest.class);

    private HttpServletRequest httpRequest;
    private OAuthMessage message;
    private OAuthAccessor accessor;

    /**
     * @param requestUrl
     *            the official URL of the request; that is the URL a legitimate
     *            client would use to compute the digital signature. If this
     *            parameter is null, this method will try to reconstruct the URL
     *            from the HTTP request; which may be wrong in some cases.
     */
    public OAuthRequest(HttpServletRequest request, String requestUrl)
            throws OAuthException, IOException {
        this.httpRequest = request;
        this.message = OAuthServlet.getMessage(httpRequest, requestUrl);

        LyoOAuthConsumer consumer =
                OAuthConfiguration.getInstance().getConsumerStore().getConsumer(message);
        if (consumer == null) {
            throw new OAuthProblemException(OAuth.Problems.CONSUMER_KEY_REJECTED);
        }

        this.accessor = new OAuthAccessor(consumer);

        // Fill in the token secret if it's there.
        String token = this.message.getToken();
        if (token != null) {
            this.accessor.tokenSecret =
                    OAuthConfiguration.getInstance()
                            .getTokenStrategy()
                            .getTokenSecret(this.httpRequest, token);
        }
    }

    public OAuthRequest(HttpServletRequest request) throws OAuthException, IOException {
        this(
                request,
                null != OAuthConfiguration.getInstance().getServletUri()
                        ? UriBuilder.fromUri(OAuthConfiguration.getInstance().getServletUri())
                                .path(request.getPathInfo())
                                .build()
                                .toString()
                        : null);
    }

    public HttpServletRequest getHttpRequest() {
        return httpRequest;
    }

    public void setHttpRequest(HttpServletRequest httpRequest) {
        this.httpRequest = httpRequest;
    }

    public OAuthMessage getMessage() {
        return message;
    }

    public OAuthAccessor getAccessor() {
        return accessor;
    }

    public LyoOAuthConsumer getConsumer() {
        return (LyoOAuthConsumer) accessor.consumer;
    }

    /**
     * Validates that the request is authorized and throws an OAuth exception if
     * not. The request must contain a valid access token and pass
     * {@link OAuthValidator#validateMessage(OAuthMessage, OAuthAccessor)} checks
     * using the validator set in the {@link OAuthConfiguration}.
     * <p>
     * If the request fails validation, you can use
     * {@link OAuthServlet#handleException(jakarta.servlet.http.HttpServletResponse, Exception, String)}
     * to send an unauthorized response.
     *
     * @throws OAuthException if the request fails validation
     */
    public void validate() throws OAuthException, IOException, ServletException {
        log.trace("validating the request.");
        try {
            OAuthConfiguration config = OAuthConfiguration.getInstance();
            config.getValidator().validateMessage(message, accessor);
            config.getTokenStrategy().validateAccessToken(this);
        } catch (URISyntaxException e) {
            throw new ServletException(e);
        } catch (OAuthProblemException e) {
            log.warn("OAuthProblemException caught when validating the request. {}", e.toString());
            throw e;
        }
    }
}
