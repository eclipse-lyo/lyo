/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-1.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client;

import javax.ws.rs.client.ClientBuilder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OslcClientBuilder {
    private String version;
    private ClientBuilder clientBuilder;

    private final static Logger log = LoggerFactory.getLogger(OslcClientBuilder.class);

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
