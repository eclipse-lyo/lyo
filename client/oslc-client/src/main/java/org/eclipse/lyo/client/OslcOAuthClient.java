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

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import javax.ws.rs.HttpMethod;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.Response;

import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.client.OAuthClient;
import net.oauth.client.httpclient4.HttpClient4;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.HttpClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OslcOAuthClient implements IOslcClient {

	private OAuthAccessor oauthAccessor;
	private String oauthRealmName;
    private UnderlyingHttpClient underlyingHttpClient;
    private OslcClient oslcClient;

    private final static Logger log = LoggerFactory.getLogger(OslcOAuthClient.class);

    /**
     * Initialize an OAuthClient with the required OAuth URLs
     * @param requestTokenURL
     * @param authorizationTokenURL
     * @param accessTokenURL
     * @param consumerKey
     * @param consumerSecret
     */
    public OslcOAuthClient(
            OAuthAccessor accessor,
            String realm,
            ClientBuilder clientBuilder,
            UnderlyingHttpClient underlyingHttpClient) {

        oauthAccessor = accessor;

        oauthRealmName = "Jazz";
        // Change if a different name was detected
        if (!StringUtils.isEmpty(realm)) {
            oauthRealmName = realm;
        }

        if (null == clientBuilder) {
            oslcClient = new OslcClient();
        }
        else {
            oslcClient = new OslcClient(clientBuilder);
        }
        this.underlyingHttpClient = underlyingHttpClient;
    }

    public OslcClient getOslcClient() {
        return oslcClient;
    }

    private Map<String, String> appendAuthorizationHeader(String url, String httpMethod, Map<String, String> requestHeaders) throws IOException, OAuthException, URISyntaxException {
        if (requestHeaders == null) {
            requestHeaders = new HashMap<>();
        }
        requestHeaders.put("Authorization", getAuthorizationHeader(url, httpMethod));
        return requestHeaders;
    }

    private String getAuthorizationHeader(String url, String httpMethod) throws IOException, OAuthException, URISyntaxException {
        if (!performedOAuthNegotiation()) {
            throw new IllegalStateException("You need to obtain an AccessToken by first calling performOAuthNegotiation()");
        }
        return oauthAccessor.newRequestMessage(httpMethod, url, null).getAuthorizationHeader(oauthRealmName);
    }

    /**
     * Check if the necessary OAuth negotiation has been performed yet.
     */
    private boolean performedOAuthNegotiation() {
        return null != oauthAccessor.accessToken;
    }

    /**
     * Performs necessary OAuth negotiation.
     *  - get request token
     *  - throw redirect exception to authorization URL (and print message)
     *  - exchange request token for access token
     *  - access protected URL and return OAuthMessage
     *
     * @param callbackURL
     * @return
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     */
    public Optional<String> performOAuthNegotiation(String callbackURL) throws IOException, OAuthException, URISyntaxException {
        return performOAuthNegotiationInternal(false, callbackURL);
    }

    private Optional<String> performOAuthNegotiationInternal(boolean restart, String callbackURL) throws IOException, OAuthException, URISyntaxException {
        //No request token yet, get the request token and throw exception to redirect for authorization.
        if (oauthAccessor.requestToken == null) {
            OAuthClient client = new OAuthClient(new HttpClient4() {
                public HttpClient getHttpClient(URL url) {
                    return underlyingHttpClient.get(oslcClient.getClient());
                }
            });
            OAuthMessage message = client.getRequestTokenResponse(oauthAccessor, OAuthMessage.GET, null);

            String userAuthorizationURL = oauthAccessor.consumer.serviceProvider.userAuthorizationURL +
            "?oauth_token=" + oauthAccessor.requestToken +
            "&oauth_callback="+URLEncoder.encode(callbackURL, "UTF-8");

            return Optional.of(userAuthorizationURL);
        }

        //Exchange request token for access token.
        if (oauthAccessor.accessToken == null) {
            try {
                OAuthClient client = new OAuthClient(new HttpClient4() {
                            public HttpClient getHttpClient(URL url) {
                                return underlyingHttpClient.get(oslcClient.getClient());
                            }
                        });
                OAuthMessage message = client.getAccessToken(oauthAccessor, OAuthMessage.POST, null);
            } catch (OAuthException e) {
                log.debug("OAuthException caught: " + e.getMessage());
                if (restart) {
                    log.error("Failed to get access key.", e);
                } else {
                    //restart the dance
                    oauthAccessor.accessToken = null;
                    oauthAccessor.requestToken = null;
                    performOAuthNegotiationInternal(true, callbackURL);
                    return Optional.empty();
                }
            }
        }
        return Optional.empty();
    }

    @Override
    public Client getClient() {
        return getOslcClient().getClient();
    }

    @Override
    public Response getResource(String url) {
        return this.getResource(url, null, OSLCConstants.CT_RDF, null, true);
    }

    @Override
    public Response getResource(String url, String mediaType) {
        return this.getResource(url, null, mediaType, null, true);
    }

    @Override
    public Response getResource(String url, Map<String, String> requestHeaders) {
        return this.getResource(url, requestHeaders, OSLCConstants.CT_RDF, null, true);
    }

    @Override
    public Response getResource(String url, Map<String, String> requestHeaders, String mediaType) {
        return this.getResource(url, requestHeaders, mediaType, null, true);
    }

    @Override
    public Response getResource(String url, Map<String, String> requestHeaders, String mediaType, String configurationContext) {
        return this.getResource(url, requestHeaders, mediaType,configurationContext, true);
    }

    @Override
    public Response getResource(String url, Map<String, String> requestHeaders, String mediaType, boolean handleRedirects) {
        return this.getResource(url, requestHeaders, mediaType, null, handleRedirects);
    }

    @Override
    public Response getResource(String url, Map<String, String> requestHeaders, String mediaType, String configurationContext, boolean handleRedirects) {
        Map<String, String> headers = null;
        try {
            headers = appendAuthorizationHeader(url, HttpMethod.GET, requestHeaders);
        } catch (IOException | OAuthException | URISyntaxException e) {
            throw new IllegalStateException(e);
        }
        return oslcClient.getResource(url, headers, mediaType,configurationContext, handleRedirects);
    }

    @Override
    public Response deleteResource(String url) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response deleteResource(String url, String configurationContext) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response createResource(String url, Object artifact, String mediaType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response createResource(String url, Object artifact, String mediaType, String acceptType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response createResource(String url, Object artifact, String mediaType, String acceptType, String configurationContext) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response updateResource(String url, Object artifact, String mediaType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response updateResource(String url, Object artifact, String mediaType, String acceptType) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response updateResource(String url, Object artifact, String mediaType, String acceptType, String ifMatch) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Response updateResource(String url, Object artifact, String mediaType, String acceptType, String ifMatch, String configurationContext) {
        throw new UnsupportedOperationException();
    }
}
