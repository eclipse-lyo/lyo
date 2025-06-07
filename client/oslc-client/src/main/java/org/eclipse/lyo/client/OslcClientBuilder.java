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

import jakarta.ws.rs.client.ClientBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OslcClientBuilder {
    private String version;
    private ClientBuilder clientBuilder;

    private static final Logger log = LoggerFactory.getLogger(OslcClientBuilder.class);

    public OslcClientBuilder() {
        version = OSLCConstants.OSLC2_0;
        clientBuilder = null;
    }

    public OslcClientBuilder setVersion(String version) {
        this.version = version;
        return this;
    }

    public OslcClientBuilder setClientBuilder(ClientBuilder clientBuilder) {
        this.clientBuilder = clientBuilder;
        return this;
    }

    public IOslcClient build() {
        return new OslcClient(clientBuilder, version);
    }
}
