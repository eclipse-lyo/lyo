/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
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
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.bugzilla.test;

import java.io.IOException;
import java.util.Properties;

import junit.framework.TestCase;

import org.eclipse.lyo.samples.bugzilla.BugzillaInitializer;
import org.eclipse.lyo.samples.bugzilla.Credentials;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetAccessibleProducts;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetLegalValues;

import com.j2bugzilla.base.BugzillaConnector;
import com.j2bugzilla.rpc.BugSearch;
import com.j2bugzilla.rpc.GetProduct;


public class TestConnection extends TestCase {

	private static Credentials credentials;
	
    static {
        Properties props = new Properties();
        try {
            props.load(BugzillaInitializer.class.getResourceAsStream("/test.properties"));
            String username    = props.getProperty("username");
            String password    = props.getProperty("password");
            System.out.println("username: "     + username);
            System.out.println("password: "     + password);
            
            credentials = new Credentials();
            credentials.setUsername(username);
            credentials.setPassword(password);
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
	public void testConnection() {
		
		try {
			BugzillaInitializer.getBugzillaConnector(credentials);
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
	}
	
//	public void testReportBug() {
//		
//		try {
//			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(credentials);
//			
//			BugFactory factory = new BugFactory();
//			Bug bug = factory.newBug().setProduct("FakePortal")
//					.setComponent("Datastore")
//					.setSummary("New Bug: " + System.currentTimeMillis())
//					.setVersion("1.0").setOperatingSystem("Mac OS")
//					.setPlatform("Macintosh").createBug();
//			ReportBug reportBug = new ReportBug(bug);			
//			bc.executeMethod(reportBug);
//			
//		} catch (Exception e) {
//			e.printStackTrace();
//			fail();
//		}
//	}
	
	public void testGetLegalValues() {
		
		Integer[] productIds = null;
		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(credentials);
			GetAccessibleProducts gap = new GetAccessibleProducts();
			bc.executeMethod(gap);
			productIds = gap.getIds();
		    assertTrue(productIds.length > 0);
			
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		
		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(credentials);
			GetProduct getProductMethod = new GetProduct(productIds[0]);
			bc.executeMethod(getProductMethod);
			assertNotNull(getProductMethod.getProduct());
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		
		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(credentials);
			GetLegalValues glv = new GetLegalValues("op_sys", -1);
			bc.executeMethod(glv);
			String[] values = glv.getValues();
		    assertTrue(values.length > 0);
		    
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
	}
	
	private final static int TEST_SEARCH_LIMIT = 5;
	
	public void testSearchBugs() {

		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(credentials);
			
			BugSearch.SearchQuery limitQuery = new BugSearch.SearchQuery(
					BugSearch.SearchLimiter.LIMIT, "" + TEST_SEARCH_LIMIT);
			
			BugSearch search = new BugSearch(limitQuery);
			
			bc.executeMethod(search);
			
			// Assumes the connected bugzilla repository has at least TEST_SEARCH_LIMIT bugs in it.
			assertEquals(TEST_SEARCH_LIMIT, search.getSearchResults().size());
			
			System.out.println("Search returned = " + search.getSearchResults().size());
			
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		
	}
}
