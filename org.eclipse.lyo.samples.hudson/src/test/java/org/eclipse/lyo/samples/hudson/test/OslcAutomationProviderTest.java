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
import hudson.model.Project;
import hudson.tasks.Shell;

import java.io.IOException;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.UriBuilder;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.AutomationConstants;
import org.jvnet.hudson.test.HudsonTestCase;

/**
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see <a href="http://wiki.eclipse.org/Writing_unit_tests_for_your_plugin">Writing unit tests for your plugin</a>
 */
public abstract class OslcAutomationProviderTest extends HudsonTestCase {
	protected <T> T getEntity(String relative, String contentType, Class<T> cls) throws Exception {
		OslcClient client = new OslcClient();
		ClientResponse response = client.getResource(urlFromRelative(relative), contentType);
		assertEquals(HttpServletResponse.SC_OK, response.getStatusCode());
		assertEquals(contentType, response.getHeaders().getFirst("Content-Type"));
		assertEquals("2.0", response.getHeaders().getFirst("OSLC-Core-Version"));
		
		T entity = response.getEntity(cls);
		assertNotNull(entity);
		
		return entity;
	}
	
	protected String urlFromRelative(String relative) throws IOException {
		return getURL().toExternalForm() + "auto" + relative;
	}
	
	protected String getJobURI(Project<?, ?> project) throws IOException {
		return getJobURI(project.getName());
	}

	protected String getJobURI(String name) throws IOException {
		UriBuilder b = UriBuilder.fromUri(urlFromRelative("/job/"));
		b.path(name);
		
		return b.build().toString();
	}
	
	protected String lookupQueryCapability(String type) throws Exception {
		OslcClient client = new OslcClient();
		return client.lookupQueryCapability(urlFromRelative("/provider"),
				AutomationConstants.AUTOMATION_DOMAIN,
				type);
	}

	protected void addShellCommand(FreeStyleProject p) throws IOException {
		p.getBuildersList().add(new Shell("echo hello"));
	}

	protected String getAutoPrefixQueryParam() {
	    return "oslc_auto=<" + AutomationConstants.AUTOMATION_NAMESPACE + ">";
	}
}
