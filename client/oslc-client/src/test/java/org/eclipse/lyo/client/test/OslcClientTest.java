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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.client.test;

import javax.ws.rs.core.Response;

import org.eclipse.lyo.client.OslcClient;
import org.junit.Test;
import static org.assertj.core.api.Assertions.*;

public class OslcClientTest {
	/*
	 * Tests that the RDF/XML MessageBodyWriter doesn't go into an infinite loop when
	 * given bad data on the client (Bug 417749). ClientRuntimeException expected.
	 */
//	@Ignore("Unit test actually POSTs data to example.com, which we shouldn't do as we " +
//			"don't own that domain. It also fails in our Hudson build environment.")
//	@Test(expected = ClientErrorException.class, timeout = 5000)
//	public void postInvalidOlscResource() throws IOException, URISyntaxException {
//		final OslcClient client = new OslcClient();
//		final AutomationRequest request = new AutomationRequest();
//
//		// Causes NullPointerException.
//		request.getExtendedProperties().put(new QName("http://example.com/ns#", "test"), null);
//
//		client.createResource("http://example.com/resources/factory", request, OSLCConstants.CT_RDF);
//	}

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
