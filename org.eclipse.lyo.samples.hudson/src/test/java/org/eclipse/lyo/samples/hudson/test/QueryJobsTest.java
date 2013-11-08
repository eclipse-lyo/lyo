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

import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.AutomationPlan;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.eclipse.lyo.oslc4j.automation.AutomationConstants;

/**
 * Tests for the Hudson Jobs query capability (OSLC AutomationPlan).
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class QueryJobsTest extends OslcAutomationProviderTest {
	public void testEmtpyQuery() throws Exception {
		OslcQueryParameters queryParams = new OslcQueryParameters();
		OslcQueryResult result = runQuery(queryParams);
		Iterable<AutomationPlan> plans = result.getMembers(AutomationPlan.class);
		assertFalse("Query result should be empty", plans.iterator().hasNext());
	}
	
	public void testQuery() throws Exception {
		// Create two projects and then query for them.
		createFreeStyleProject();
		createFreeStyleProject();
		
		OslcQueryParameters queryParams = new OslcQueryParameters();
		OslcQueryResult result = runQuery(queryParams);
		assertEquals("Expected two projects", 2, result.getMembersUrls().length);
	}
	
	// TODO: Add JUnit tests when oslc.where support is implemented.

	protected OslcQueryResult runQuery(OslcQueryParameters queryParams) throws Exception {
		OslcClient client = new OslcClient();
		String queryCapability = lookupQueryCapability();
		OslcQuery query = new OslcQuery(client, queryCapability, queryParams);
		OslcQueryResult result = query.submit();
		
		return result;
	}
	
	protected String lookupQueryCapability() throws Exception {
		return lookupQueryCapability(AutomationConstants.TYPE_AUTOMATION_PLAN);
	}
}
