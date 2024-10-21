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

import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTimeout;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.http.HttpHeaders;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import jakarta.ws.rs.client.Invocation.Builder;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status.Family;

public class OslcClientTest {
    /**
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
            Response response = client.createResource(
                    "http://open-services.net/.well-known/resource-that-should-not-exist-whose-status-code-should-not-be-200",
                    request, OSLCConstants.CT_RDF);
            assertThat(response.getStatusInfo().getFamily() != Family.SUCCESSFUL);
            // assertThrows(ClientErrorException.class, () -> {
            //
            // });
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

    @Test
    public void testGetResource() {
        OslcClient client = mock(OslcClient.class, Mockito.CALLS_REAL_METHODS);
        doReturn(null).when(client).doRequest(any(), any(), any(), any(), any(), any(), any(), any());
        client.getResource("test.url");
        verify(client).doRequest("GET", "test.url", null, null, null, null, "application/rdf+xml", null);

        clearInvocations(client);
        client.getResource("test.url", "application/rdf+xml");
        verify(client).doRequest("GET", "test.url", null, null, null, "application/rdf+xml", "application/rdf+xml",
                null);

        clearInvocations(client);
        client.getResource("test.url", Map.of("a", "b"), "application/rdf+xml", "oslc.context");
        verify(client).doRequest("GET", "test.url", null, "oslc.context", null, "application/rdf+xml",
                "application/rdf+xml", Map.of("a", "b"));
    }

    @Test
    public void testPutResource() {
        OslcClient client = mock(OslcClient.class, Mockito.CALLS_REAL_METHODS);
        doReturn(null).when(client).doRequest(any(), any(), any(), any(), any(), any(), any(), any());

        client.updateResource("test.url", "artifact", "application/json");
        verify(client).doRequest("PUT", "test.url", "artifact", null, null, "application/json", "*/*", null);

        clearInvocations(client);
        client.updateResource("test.url", "artifact", "application/json", "*/*", "ifmatch", "configContext");
        verify(client).doRequest("PUT", "test.url", "artifact", "configContext", "ifmatch", "application/json", "*/*",
                null);


        clearInvocations(client);
        client.updateResource("test.url", "artifact", "application/json", "*/*", "ifmatch");
        verify(client).doRequest("PUT", "test.url", "artifact", null, "ifmatch", "application/json", "*/*", null);
    }

    @Test
    public void testDeleteResource() {
        OslcClient client = mock(OslcClient.class, Mockito.CALLS_REAL_METHODS);
        doReturn(null).when(client).doRequest(any(), any(), any(), any(), any(), any(), any(), any());

        client.deleteResource("test.url");
        verify(client).doRequest("DELETE", "test.url", null, null, null, null, null, null);

        clearInvocations(client);
        client.deleteResource("test.url", "configContext");
        verify(client).doRequest("DELETE", "test.url", null, "configContext", null, null, null, null);

    }

    @Test
    public void testCreateResource() {
        OslcClient client = mock(OslcClient.class, Mockito.CALLS_REAL_METHODS);
        doReturn(null).when(client).doRequest(any(), any(), any(), any(), any(), any(), any(), any());

        client.createResource("test.url", "artifact", "application/rdf+xml");
        verify(client).doRequest("POST", "test.url", "artifact", null, null, "application/rdf+xml", "*/*", null);

        clearInvocations(client);
        client.createResource("test.url", "artifact", "application/rdf+xml", "*/*", "oslc.ctx");
        verify(client).doRequest("POST", "test.url", "artifact", "oslc.ctx", null, "application/rdf+xml", "*/*", null);
    }

    @Test
    public void testAddHeaders() {
        OslcClient client = new OslcClient();
        Builder builder = mock(Builder.class);
        Map<String, String> headers = client.addHeaders(builder, Map.of("a", "b"), "ifmatch", "ctx");
        assertEquals(4, headers.size());

        assertEquals("b", headers.get("a"));
        assertEquals("ctx", headers.get(OSLCConstants.CONFIGURATION_CONTEXT_HEADER));
        assertEquals("ifmatch", headers.get(HttpHeaders.IF_MATCH));
        assertEquals("2.0", headers.get(OSLCConstants.OSLC_CORE_VERSION));


        headers = client.addHeaders(builder, Map.of("a", "b"), "ifmatch", null);
        assertEquals(3, headers.size());

        assertEquals("b", headers.get("a"));
        assertNull(headers.get(OSLCConstants.CONFIGURATION_CONTEXT_HEADER));
        assertEquals("ifmatch", headers.get(HttpHeaders.IF_MATCH));
        assertEquals("2.0", headers.get(OSLCConstants.OSLC_CORE_VERSION));
    }
}
