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

import hudson.model.FreeStyleBuild;
import hudson.model.FreeStyleProject;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.AutomationConstants;
import org.eclipse.lyo.client.oslc.resources.AutomationResult;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;

public class QueryRunsTest extends OslcAutomationProviderTest {
	public void testEmtpyQuery() throws Exception {
		OslcQueryParameters queryParams = new OslcQueryParameters();
		OslcQueryResult result = queryRuns(queryParams);
		Iterable<AutomationResult> results = result.getMembers(AutomationResult.class);
		assertFalse("Query result should be empty", results.iterator().hasNext());
	}
	
	public void testQuery() throws Exception {
		// Create some projects and runs.
		FreeStyleProject project1 = createFreeStyleProject();
		addShellCommand(project1);
		scheduleBuildAndWait(project1);
		scheduleBuildAndWait(project1);

		FreeStyleProject project2 = createFreeStyleProject();
		addShellCommand(project2);
		scheduleBuildAndWait(project2);
		
		OslcQueryParameters queryParams = new OslcQueryParameters();
		OslcQueryResult result = queryRuns(queryParams);
		assertEquals("Expected three runs", 3, result.getMembersUrls().length);
	}
	
	public void testWhereReportsOnPlan() throws Exception {
		// Create some projects and runs.
		FreeStyleProject project1 = createFreeStyleProject("project1");
		addShellCommand(project1);
		scheduleBuildAndWait(project1);
		scheduleBuildAndWait(project1);

		FreeStyleProject project2 = createFreeStyleProject();
		addShellCommand(project2);
		scheduleBuildAndWait(project2);
		
		OslcQueryParameters queryParams = new OslcQueryParameters();
		queryParams.setPrefix(getAutoPrefixQueryParam());
		String jobURI = getJobURI("project1");
		queryParams.setWhere("oslc_auto:reportsOnAutomationPlan=<" + jobURI + ">");
		OslcQueryResult result = queryRuns(queryParams);
		assertEquals("Expected two runs", 2, result.getMembersUrls().length);
	}
	
	public void testWhereNoResults() throws Exception {
		// Create some projects and runs.
		FreeStyleProject project1 = createFreeStyleProject("project1");
		addShellCommand(project1);
		scheduleBuildAndWait(project1);
		
		OslcQueryParameters queryParams = new OslcQueryParameters();
		queryParams.setPrefix(getAutoPrefixQueryParam());
		queryParams.setWhere("oslc_auto:reportsOnAutomationPlan=<http://example.com/job/foo>");
		OslcQueryResult result = queryRuns(queryParams);
		assertEquals("Expected no runs", 0, result.getMembersUrls().length);
	}

	protected void scheduleBuildAndWait(FreeStyleProject p) throws InterruptedException, ExecutionException {
		Future<FreeStyleBuild> f = p.scheduleBuild2(0);
		if (f == null) {
			fail("Project " + p.getName() + " is not buildable.");
		}
		f.get();
	}

	protected OslcQueryResult queryRuns(OslcQueryParameters queryParams) throws Exception {
		OslcClient client = new OslcClient();
		String queryCapability = lookupQueryCapability();
		OslcQuery query = new OslcQuery(client, queryCapability, queryParams);
		System.err.println("Query URL: " + query.getQueryUrl());
		OslcQueryResult result = query.submit();
		
		return result;
	}
	
	protected String lookupQueryCapability() throws Exception {
		return lookupQueryCapability(AutomationConstants.TYPE_AUTOMATION_RESULT);
	}
}
