/*
 * Copyright (c) 2018 Andrew Berezovskyi.
 *
 * All rights reserved. This program and the accompanying materials are made available under the terms of the Eclipse
 * Public License v1.0 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html and the Eclipse Distribution
 * License is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 * Andrew Berezovskyi    -  Initial implementation
 */

package org.eclipse.lyo.oslc4j.trs.client.config;

import java.util.concurrent.ScheduledExecutorService;
import org.eclipse.lyo.oslc4j.trs.client.util.TrsBasicAuthOslcClient;

/**
 * Created on 2018-02-27
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public class TrsConsumerConfiguration {
    private final String sparqlQueryUrl;
    private final String sparqlUpdateUrl;
    private final String sparqlUsername;
    private final String sparqlPassword;
    private final TrsBasicAuthOslcClient httpClient;
    private final String mqttClientId;
    private final ScheduledExecutorService scheduler;

    public TrsConsumerConfiguration(final String sparqlQueryUrl, final String sparqlUpdateUrl,
            final String sparqlUsername, final String sparqlPassword, final TrsBasicAuthOslcClient httpClient,
            final String mqttClientId, final ScheduledExecutorService scheduler) {
        this.sparqlQueryUrl = sparqlQueryUrl;
        this.sparqlUpdateUrl = sparqlUpdateUrl;
        this.sparqlUsername = sparqlUsername;
        this.sparqlPassword = sparqlPassword;
        this.httpClient = httpClient;
        this.mqttClientId = mqttClientId;
        this.scheduler = scheduler;
    }

    public ScheduledExecutorService getScheduler() {
        return scheduler;
    }

    public String getMqttClientId() {
        return mqttClientId;
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

    public TrsBasicAuthOslcClient getHttpClient() {
        return httpClient;
    }
}
