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

import hudson.model.FreeStyleProject;

import javax.servlet.http.HttpServletResponse;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.AutomationPlan;

public class JobTest extends OslcAutomationProviderTest {
	public void testNotFound() throws Exception {
		OslcClient client = new OslcClient();
		String bogusURI = getJobURI("bogus");
		ClientResponse response = client.getResource(bogusURI, "application/rdf+xml");
		assertEquals(HttpServletResponse.SC_NOT_FOUND, response.getStatusCode());
	}
	
	public void testGetXml() throws Exception {
		testGetProject(OSLCConstants.CT_XML);
	}
	
	public void testGetRdfXml() throws Exception {
		testGetProject(OSLCConstants.CT_RDF);
	}

	public void testGetTurtle() throws Exception {
		testGetProject("text/turtle");
	}
	
	public void testNotAcceptable() throws Exception {
		FreeStyleProject p = createFreeStyleProject();
		OslcClient client = new OslcClient();
		ClientResponse response = client.getResource(getJobURI(p), "application/bogus");
		assertEquals(HttpServletResponse.SC_NOT_ACCEPTABLE, response.getStatusCode());
	}

	private void testGetProject(String mediaType) throws Exception {
		FreeStyleProject p = createFreeStyleProject();
		OslcClient client = new OslcClient();
		ClientResponse response = client.getResource(getJobURI(p), mediaType);
		assertEquals(HttpServletResponse.SC_OK, response.getStatusCode());
		AutomationPlan plan = response.getEntity(AutomationPlan.class);
		assertEquals(p.getName(), plan.getTitle());
	}
}
