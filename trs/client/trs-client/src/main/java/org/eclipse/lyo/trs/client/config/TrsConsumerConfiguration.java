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

package org.eclipse.lyo.trs.client.config;

import com.google.common.base.Strings;
import java.util.concurrent.ScheduledExecutorService;
import javax.ws.rs.client.ClientBuilder;
import org.eclipse.lyo.client.OslcClient;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;

public class TrsConsumerConfiguration {
    private final String sparqlQueryUrl;
    private final String sparqlUpdateUrl;
    private final String sparqlUsername;
    private final String sparqlPassword;
    private final ScheduledExecutorService scheduler;
    private final String basicUsername;
    private final String basicPassword;
    private OslcClient httpClient;

    public TrsConsumerConfiguration(final String sparqlQueryUrl, final String sparqlUpdateUrl,
            final String sparqlUsername, final String sparqlPassword,
            final ScheduledExecutorService scheduler, final String basicUsername,
            final String basicPassword) {
        this.sparqlQueryUrl = sparqlQueryUrl;
        this.sparqlUpdateUrl = sparqlUpdateUrl;
        this.sparqlUsername = sparqlUsername;
        this.sparqlPassword = sparqlPassword;
        this.scheduler = scheduler;
        this.basicUsername = basicUsername;
        this.basicPassword = basicPassword;
    }

    public ScheduledExecutorService getScheduler() {
        return scheduler;
    }

    public String getSparqlQueryUrl() {
        return sparqlQueryUrl;
    }

    public String getSparqlUpdateUrl() {
        return sparqlUpdateUrl;
    }

    public String getSparqlUsername() {
        return sparqlUsername;
    }

    public String getSparqlPassword() {
        return sparqlPassword;
    }

    // TODO Andrew@2019-07-15: create a client factory per domain or something similar
    public OslcClient getHttpClient() {
        if (httpClient == null) {
            final ClientBuilder builder = ClientBuilder.newBuilder();
            if (!Strings.isNullOrEmpty(basicUsername)) {
                builder.register(HttpAuthenticationFeature.basic(basicUsername, basicPassword));
            }
            httpClient = new OslcClient(builder);
        }
        return httpClient;
    }
}
