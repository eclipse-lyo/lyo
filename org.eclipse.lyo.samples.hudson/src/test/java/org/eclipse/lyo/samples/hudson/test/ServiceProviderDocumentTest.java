/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Samuel Padgett - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.hudson.test;

import java.net.URI;

import javax.servlet.http.HttpServletResponse;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.AutomationConstants;
import org.eclipse.lyo.oslc4j.core.model.Dialog;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;

/**
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class ServiceProviderDocumentTest extends OslcAutomationProviderTest {
	public void testServiceProviderDocumentXml() throws Exception {
		testServiceProviderDocument(OSLCConstants.CT_XML);
	}

	public void testServiceProviderDocumentRdfXml() throws Exception {
		testServiceProviderDocument(OSLCConstants.CT_RDF);
	}

	public void testServiceProviderDocumentTurtle() throws Exception {
		testServiceProviderDocument("text/turtle");
	}
	
	public void testNotAcceptable() throws Exception {
		OslcClient client = new OslcClient();
		ClientResponse response = client.getResource(urlFromRelative("/provider"), "application/bogus");
		assertEquals(HttpServletResponse.SC_NOT_ACCEPTABLE, response.getStatusCode());
	}
	
	private void testServiceProviderDocument(String contentType) throws Exception {
		ServiceProvider p = getEntity("/provider", contentType, ServiceProvider.class);
	
		assertEquals("OSLC Automation Provider for Hudson and Jenkins", p.getTitle());
		Service[] services = p.getServices();
		assertEquals(1, services.length);
		Service service = services[0];

		URI[] usages = service.getUsages();
		assertEquals(1, usages.length);
		assertEquals(AutomationConstants.AUTOMATION_NAMESPACE + "Build", usages[0].toString());

		// Other tests actually use the document to lookup these services, so no
		// need to go crazy testing them here. Just a sanity check that the doc
		// has what we expect.
		assertEquals(2, service.getQueryCapabilities().length);
		assertEquals(1, service.getCreationFactories().length);
		assertEquals(1, service.getSelectionDialogs().length);
		
		Dialog selection = service.getSelectionDialogs()[0];
		assertNotNull(selection.getHintHeight());
		assertNotNull(selection.getHintWidth());

		URI selectionURI = selection.getDialog();
		createWebClient().getPage(selectionURI.toURL());
	}
}
