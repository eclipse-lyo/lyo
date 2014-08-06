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
package org.eclipse.lyo.client.test;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.xml.namespace.QName;

import net.oauth.OAuthException;

import org.apache.wink.client.ClientRuntimeException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.AutomationRequest;
import org.junit.Ignore;
import org.junit.Test;

public class OslcClientTest {
	/*
	 * Tests that the RDF/XML MessageBodyWriter doesn't go into an infinite loop when
	 * given bad data on the client (Bug 417749). ClientRuntimeException expected.
	 */
	@Ignore // ignore this test because it actually connects to example.com, which fails in our Hudson environment
	@Test(expected = ClientRuntimeException.class, timeout = 5000)
	public void postInvalidOlscResource() throws IOException, OAuthException, URISyntaxException {
		final OslcClient client = new OslcClient();
		final AutomationRequest request = new AutomationRequest();

		// Causes NullPointerException.
		request.getExtendedProperties().put(new QName("http://example.com/ns#", "test"), null);

		client.createResource("http://example.com/resources/factory", request, OSLCConstants.CT_RDF);
	}
}
