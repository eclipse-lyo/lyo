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

import hudson.Launcher;
import hudson.model.BuildListener;
import hudson.model.FreeStyleBuild;
import hudson.model.AbstractBuild;
import hudson.model.BooleanParameterDefinition;
import hudson.model.FreeStyleProject;
import hudson.model.ParameterDefinition;
import hudson.model.ParametersDefinitionProperty;
import hudson.model.StringParameterDefinition;
import hudson.util.OneShotEvent;
import hudson.util.RunList;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import net.oauth.OAuthException;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.AutomationConstants;
import org.eclipse.lyo.client.oslc.resources.AutomationPlan;
import org.eclipse.lyo.client.oslc.resources.AutomationRequest;
import org.eclipse.lyo.client.oslc.resources.ParameterInstance;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.jvnet.hudson.test.TestBuilder;

public class ScheduleBuildTest extends OslcAutomationProviderTest {
	public void testScheduleBuildXml() throws Exception {
		testSimpleScheduleBuild(OSLCConstants.CT_XML);
	}

	public void testScheduleBuildRdfXml() throws Exception {
		testSimpleScheduleBuild(OSLCConstants.CT_RDF);
	}

	public void testScheduleBuildTurtle() throws Exception {
		testSimpleScheduleBuild("text/turtle");
	}
	
	public void testScheduleParameterizedBuild() throws Exception {
		final OneShotEvent buildStarted = new OneShotEvent();

		FreeStyleProject project = createFreeStyleProject("MyProject");
		project.setQuietPeriod(1);
		project.getBuildersList().add(new TestBuilder() {
		    public boolean perform(AbstractBuild<?, ?> build, Launcher launcher,
		        BuildListener listener) throws InterruptedException, IOException {
		        buildStarted.signal();
		        return true;
		    }
		});

		ArrayList<ParameterDefinition> parameterDefinitions = new ArrayList<ParameterDefinition>();
		parameterDefinitions.add(new StringParameterDefinition("string", "foo"));
		parameterDefinitions.add(new BooleanParameterDefinition("bool", true, "A boolean parameter."));
		ParametersDefinitionProperty params = new ParametersDefinitionProperty(parameterDefinitions);
		project.addProperty(params); 

		AutomationPlan plan = getEntity("/job/MyProject", OSLCConstants.CT_RDF, AutomationPlan.class);
		assertEquals("Expected two parameter definitions", 2, plan.getParameterDefinitions().length);
		
		AutomationRequest request = new AutomationRequest();
		request.setExecutesAutomationPlan(new Link(plan.getAbout()));
		
		ParameterInstance stringParameter = new ParameterInstance();
		stringParameter.setName("string");
		stringParameter.setValue("bar");
		request.addInputParameter(stringParameter);
		// TODO: Test other types.
		
		ClientResponse response = createAutomationRequest(request, OSLCConstants.CT_RDF);
		assertEquals(HttpServletResponse.SC_CREATED, response.getStatusCode());
		String location = response.getHeaders().getFirst("Location");
		assertNotNull(location);
		
		// Wait for the build.
		buildStarted.block(10000);

		// FIXME: Should be able to get the automation request before the build starts!
		OslcClient client = new OslcClient();
		response = client.getResource(location, OSLCConstants.CT_RDF);
		assertEquals(HttpServletResponse.SC_OK, response.getStatusCode());
		AutomationRequest fetchedRequest = response.getEntity(AutomationRequest.class);
		assertEquals(location, fetchedRequest.getAbout().toString());
		assertEquals(plan.getAbout(), fetchedRequest.getExecutesAutomationPlan().getValue());
	
		RunList<FreeStyleBuild> runs = project.getBuilds();
		assertEquals(1, runs.size());
		FreeStyleBuild build = runs.iterator().next();
		Map<String, String> actualParameters = build.getBuildVariables();
		assertEquals("bar", actualParameters.get("string"));
	}

	private void testSimpleScheduleBuild(String mediaType) throws Exception {
		// Create some projects and runs.
		FreeStyleProject project = createFreeStyleProject();
		addShellCommand(project);

		AutomationRequest request = new AutomationRequest();
		String executes = getJobURI(project);
		request.setExecutesAutomationPlan(new Link(new URI(executes)));

		ClientResponse response = createAutomationRequest(request, OSLCConstants.CT_RDF);
		assertEquals(HttpServletResponse.SC_CREATED, response.getStatusCode());
		assertNotNull(response.getHeaders().getFirst("Location"));
	}
	
	private ClientResponse createAutomationRequest(AutomationRequest request,
	        String mediaType) throws Exception {
		OslcClient client = new OslcClient();
		String factory = lookupCreationFactory(client);

		return client.createResource(factory, request, mediaType);
	}
	
	private String lookupCreationFactory(OslcClient client) throws IOException,
            OAuthException, URISyntaxException, ResourceNotFoundException {
		return client.lookupCreationFactory(urlFromRelative("/provider"),
		        AutomationConstants.AUTOMATION_DOMAIN,
		        AutomationConstants.TYPE_AUTOMATION_REQUEST);
    }
}
