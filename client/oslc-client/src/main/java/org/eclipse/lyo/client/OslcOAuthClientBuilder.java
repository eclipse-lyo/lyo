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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.ws.rs.client.ClientBuilder;
import net.oauth.OAuthAccessor;
import net.oauth.OAuthConsumer;
import net.oauth.OAuthServiceProvider;

public class OslcOAuthClientBuilder {
    private String requestTokenURL;
    private String authorizationTokenURL;
    private String accessTokenURL;

    private String callback;
    private String consumerKey;
    private String consumerSecret;

    private String oauthRealmName;

    private ClientBuilder clientBuilder;
    private UnderlyingHttpClient underlyingHttpClient;

    private final static Logger log = LoggerFactory.getLogger(OslcOAuthClientBuilder.class);

    public OslcOAuthClientBuilder() {
        requestTokenURL = "";
        authorizationTokenURL = "";
        accessTokenURL = "";
        callback = "";
        consumerKey = "";
        consumerSecret = "";
        oauthRealmName = "";
        clientBuilder = null;
        underlyingHttpClient = null;

    }

    public OslcOAuthClientBuilder setFromRootService(RootServicesHelper rootService) {
        this.setOAuthServiceProvider(rootService.getRequestTokenUrl(), rootService.getAuthorizationTokenUrl(), rootService.getAccessTokenUrl());
        this.setOauthRealmName(rootService.getAuthorizationRealm());
        return this;
    }

    public OslcOAuthClientBuilder setOAuthServiceProvider(String requestTokenURL, String authorizationTokenURL, String accessTokenURL) {
        this.requestTokenURL = requestTokenURL;
        this.authorizationTokenURL = authorizationTokenURL;
        this.accessTokenURL = accessTokenURL;
        return this;
    }

    public OslcOAuthClientBuilder setOAuthConsumer(String callback, String consumerKey, String consumerSecret) {
        this.callback= callback;
        this.consumerKey = consumerKey;
        this.consumerSecret = consumerSecret;
        return this;
    }

    public OslcOAuthClientBuilder setOauthRealmName(String oauthRealmName) {
        this.oauthRealmName = oauthRealmName;
        return this;
    }

    public OslcOAuthClientBuilder setCallback(String callback) {
        this.callback = callback;
        return this;
    }

    public OslcOAuthClientBuilder setClientBuilder(ClientBuilder clientBuilder) {
        this.clientBuilder = clientBuilder;
        return this;
    }

    public OslcOAuthClientBuilder setUnderlyingHttpClient(UnderlyingHttpClient underlyingHttpClient) {
        this.underlyingHttpClient = underlyingHttpClient;
        return this;
    }

    public IOslcClient build() {
        OAuthServiceProvider oAuthServiceProvider = new OAuthServiceProvider(requestTokenURL, authorizationTokenURL, accessTokenURL);
        OAuthConsumer oAuthConsumer = new OAuthConsumer(callback, consumerKey, consumerSecret, oAuthServiceProvider);
        OAuthAccessor oauthAccessor = new OAuthAccessor(oAuthConsumer);
        return new OslcOAuthClient(oauthAccessor, oauthRealmName, clientBuilder, underlyingHttpClient);
    }

}
