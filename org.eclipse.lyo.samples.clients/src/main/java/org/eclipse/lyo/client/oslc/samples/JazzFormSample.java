/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Michael Fiedler     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URLEncoder;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.apache.http.HttpStatus;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.EntityType;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.jazz.JazzFormAuthClient;
import org.eclipse.lyo.client.oslc.jazz.JazzRootServicesHelper;
import org.eclipse.lyo.client.oslc.resources.ChangeRequest;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.CommandLineParser;


public class JazzFormSample {
	
	private static final Set<Class<?>> PROVIDERS = new HashSet<Class<?>>();


	public static void main(String[] args) {
		
		Options options=new Options();
		
		options.addOption("url", true, "url");
		options.addOption("user", true, "user ID");
		options.addOption("password", true, "password");
		options.addOption("project",true,"project area");

		
		CommandLineParser cliParser = new GnuParser();
		
		try {
			
			//Parse the command line
			CommandLine cmd = cliParser.parse(options, args);
			String rootServicesUrl = cmd.getOptionValue("url");
			String user = cmd.getOptionValue("user");
			String passwd = cmd.getOptionValue("password");
			String projectArea = cmd.getOptionValue("project");
			
			//Initialize a Jazz rootservices helper and indicate we're looking for the ChangeManagement catalog
			//RTC contains a service provider for CM and SCM, so we need to indicate our interest in CM
			JazzRootServicesHelper helper = new JazzRootServicesHelper(rootServicesUrl,OSLCConstants.OSLC_CM_V2);
			
			//Create a new Form Auth client 
			JazzFormAuthClient client = helper.initFormClient(user, passwd);
			
			//Login in to Jazz Server
			if (client.formLogin() == HttpStatus.SC_OK) {
				
				//Get the URL of the ChangeManagement catalog
				String catalogUrl = helper.getCatalogUrl();
				
				//Find the Service provider for the project area we want to work with
				String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, projectArea);
				
				//Get the Query Capabilities URL so that we can run some OSLC queries
				String queryCapability = client.lookupQueryCapability(serviceProviderUrl,OSLCConstants.OSLC_CM_V2);
				
				OslcQuery query = new OslcQuery(client, queryCapability, 5);
				System.out.println("base query = " + query.getCapabilityUrl());
				System.out.println("full query = " + query.getQueryUrl());
				OslcQueryResult result = query.submit();
				
				int page = 1;
				do {
					System.out.println("Page " + page);
					for (String resultsUrl : result.getMembersUrls()) {
						System.out.println(resultsUrl);
					}
					if (result.hasNext()) {
						result = result.next();
						page++;
					} else {
						break;
					}
				} while(true);
				
				/*
				//First let's get run a query to get references to 50 workitems (default page size) in the project area.
				//TODO: Add paging support
				//To do this, just do a GET on the Query Capability URL	
//				String queryUrl = queryCapability;
//				ClientResponse response = client.getResponse(queryUrl,"application/rdf+xml");
				
				Collection<String> changeRequestRefs = client.getQueryResponseMembers(query.getCapabilityUrl(), response);	
				
				//Now, loop through each of the references in the query response and get the actual ChangeRequest
				//Note: If we could use oslc.select=* with RTC, we would not have to do these followup requests.
				//We could have retrieved everything at once.   This is defect https://jazz.net/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/210291
				
				System.out.println("Getting ready to retrieve " + changeRequestRefs.size() + " change requests");
				for (String changeRequestRef : changeRequestRefs ) {
					response = client.getResponse(changeRequestRef, "application/rdf+xml");
					
					//Marshal as an OSLC4J ChangeRequest.  If you'd prefer the raw XML for the workitem, comment the next 2 lines and
					//uncomment the code below.
					ChangeRequest cr = response.getEntity(ChangeRequest.class);
					printChangeRequestInfo(cr);   //print a few attributes
					
					//Uncomment following code if you prefer to work with the raw XML
//					InputStream is = response.getEntity(InputStream.class);
//					BufferedReader in = new BufferedReader(new InputStreamReader(is));
//					String line = null;
//					while((line = in.readLine()) != null) {
//					  System.out.println(line);
//					}
//					System.out.println();
					
				}
				*/
				
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
		


	}
	
	private static void printChangeRequestInfo(ChangeRequest cr) {
		//See the OSLC4J ChangeRequest class in the OSLC4JChangeManagementCommon project for a full list of attributes you can access.
		System.out.println("ID: " + cr.getIdentifier() + ", Title: " + cr.getTitle() + ", Status: " + cr.getStatus());
	}

}
