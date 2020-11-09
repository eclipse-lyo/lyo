/*******************************************************************************
 * Copyright (c) 2013, 2014 IBM Corporation and others.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *  Contributors:
 *
 *     Samuel Padgett                  - initial API and implementation
 *     Samuel Padgett                  - set timeout on postInvalidOslcResource()
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.client.test;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.ws.rs.core.Response;
import javax.xml.namespace.QName;

import javax.ws.rs.ClientErrorException;

import org.eclipse.lyo.oslc4j.client.OSLCConstants;
import org.eclipse.lyo.oslc4j.client.OslcClient;
import org.junit.Ignore;
import org.junit.Test;
import static org.assertj.core.api.Assertions.*;

public class OslcClientTest {
	/*
	 * Tests that the RDF/XML MessageBodyWriter doesn't go into an infinite loop when
	 * given bad data on the client (Bug 417749). ClientRuntimeException expected.
	 */
	@Ignore("Unit test actually POSTs data to example.com, which we shouldn't do as we " +
			"don't own that domain. It also fails in our Hudson build environment.")
	@Test(expected = ClientErrorException.class, timeout = 5000)
	public void postInvalidOlscResource() throws IOException, URISyntaxException {
		final OslcClient client = new OslcClient();
		final AutomationRequest request = new AutomationRequest();

		// Causes NullPointerException.
		request.getExtendedProperties().put(new QName("http://example.com/ns#", "test"), null);

		client.createResource("http://example.com/resources/factory", request, OSLCConstants.CT_RDF);
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
