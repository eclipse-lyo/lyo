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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import jbugz.base.Bug;
import jbugz.base.BugzillaConnector;
import jbugz.rpc.ReportBug;
import junit.framework.TestCase;

import org.eclipse.lyo.samples.bugzilla.BugzillaInitializer;
import org.eclipse.lyo.samples.bugzilla.Credentials;
import org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.ExtendedBugSearch;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetAccessibleProducts;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetLegalValues;
import org.eclipse.lyo.samples.bugzilla.jbugzx.rpc.GetProducts;


public class TestConnection extends TestCase {

	private static Credentials credentials;
	
    static {
        Properties props = new Properties();
        try {
            props.load(BugzillaInitializer.class.getResourceAsStream("/bugz.properties"));
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
	
	public void testReportBug() {
		
		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(credentials);
			
			Map<String, Object> bugState = new HashMap<String, Object>();
			bugState.put("product", "FakePortal");
			bugState.put("component", "Datastore");
			bugState.put("summary", "New Bug: " + System.currentTimeMillis());
			bugState.put("version", "1.0");
			bugState.put("op_sys", "Mac OS");
			bugState.put("platform", "Macintosh");
			Bug bug = new  Bug(bugState);
			ReportBug reportBug = new ReportBug(bug);
			
			bc.executeMethod(reportBug);
			
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
	}
	
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
			GetProducts gps = new GetProducts(productIds);
			bc.executeMethod(gps);
			List<Product> products = gps.getProducts();
		    assertTrue(products.size()  > 0);
		    
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
	
	public void testSearchBugs() {

		try {
			BugzillaConnector bc = BugzillaInitializer.getBugzillaConnector(credentials);
			
			ExtendedBugSearch search = new ExtendedBugSearch(ExtendedBugSearch.ExtendedSearchLimiter.PRODUCT, "FakePortal");
			bc.executeMethod(search);
			
			System.out.println("Search returned = " + search.getSearchResults().size());
			
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		
	}
}
