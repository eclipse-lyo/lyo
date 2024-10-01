/*
 * Copyright (c) 2021 Contributors to the Eclipse Foundation
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

import static java.time.Duration.ofSeconds;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTimeout;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.junit.jupiter.api.Test;

import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status.Family;

public class OslcClientTest {
    /*
     * Tests that the RDF/XML MessageBodyWriter doesn't go into an infinite loop when
     * given bad data on the client (Bug 417749). ClientRuntimeException no longer expected in Lyo 4.0.
     */
//    @Disabled("Unit test actually POSTs data to example.com, which we shouldn't do as we don't own that domain.")
    @Test
    public void postInvalidOlscResource() throws IOException, URISyntaxException {
        assertTimeout(ofSeconds(10), () -> {
            final OslcClient client = new OslcClient();
            final ServiceProvider request = new ServiceProvider();
            request.getExtendedProperties().put(new QName("http://example.com/ns#", "test"), "test");
            Response response = client.createResource("http://open-services.net/.well-known/resource-that-should-not-exist-whose-status-code-should-not-be-200", request, OSLCConstants.CT_RDF);
            assertThat(response.getStatusInfo().getFamily() != Family.SUCCESSFUL);
//            assertThrows(ClientErrorException.class, () -> {
//
//            });
        });
    }

    @Test
    public void initTest() {
        final OslcClient client = new OslcClient();
        assertThat(client).isNotNull();
    }

    @Test
    public void connectionTest() {
        final OslcClient client = new OslcClient();
        final Response resource = client.getResource("https://open-services.net");
        assertThat(resource).isNotNull();
        assertThat(resource.getStatus()).isLessThan(400);
    }
}
