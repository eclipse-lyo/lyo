/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *     Steve Pitschke      - added use of new OslcQueryResult.getMembers()
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.jazz.JazzFormAuthClient;
import org.eclipse.lyo.client.oslc.jazz.JazzRootServicesHelper;
import org.eclipse.lyo.client.oslc.resources.ChangeRequest;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * Samples of logging in to Rational Team Concert and running OSLC operations
 * 
 * 
 * - run an OLSC ChangeRequest query and retrieve OSLC ChangeRequests and de-serialize them as Java objects
 * - retrieve an OSLC ChangeRequest and print it as XML
 * - create a new ChangeRequest
 * - update an existing ChangeRequest
 *
 */
public class RTCFormSample {

	private static final Logger logger = Logger.getLogger(RTCFormSample.class.getName());

	/**
	 * Login to the RTC server and perform some OSLC actions
	 * @param args
	 * @throws ParseException 
	 */
	public static void main(String[] args) throws ParseException {

		Options options=new Options();
		
		options.addOption("url", true, "url");
		options.addOption("user", true, "user ID");
		options.addOption("password", true, "password");
		options.addOption("project",true,"project area");

		CommandLineParser cliParser = new GnuParser();			
		
		//Parse the command line
		CommandLine cmd = cliParser.parse(options, args);
		
		if (!validateOptions(cmd)) {		
			logger.severe("Syntax:  java <class_name> -url https://<server>:port/<context>/ -user <user> -password <password> -project \"<project_area>\"");
			logger.severe("Example: java RTCFormSample -url https://exmple.com:9443/ccm -user ADMIN -password ADMIN -project \"JKE Banking (Change Management)\"");
			return;
		}
			
		String webContextUrl = cmd.getOptionValue("url");
		String user = cmd.getOptionValue("user");
		String passwd = cmd.getOptionValue("password");
		String projectArea = cmd.getOptionValue("project");
		
		try {
		
			//STEP 1: Initialize a Jazz rootservices helper and indicate we're looking for the ChangeManagement catalog
			//RTC contains a service provider for CM and SCM, so we need to indicate our interest in CM
			JazzRootServicesHelper helper = new JazzRootServicesHelper(webContextUrl,OSLCConstants.OSLC_CM_V2);
			
			//STEP 2: Create a new Form Auth client with the supplied user/password
			JazzFormAuthClient client = helper.initFormClient(user, passwd);
			
			//STEP 3: Login in to Jazz Server
			if (client.formLogin() == HttpStatus.SC_OK) {
				
				//STEP 4: Get the URL of the OSLC ChangeManagement catalog
				String catalogUrl = helper.getCatalogUrl();
				
				//STEP 5: Find the OSLC Service Provider for the project area we want to work with
				String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, projectArea);
				
				//STEP 6: Get the Query Capabilities URL so that we can run some OSLC queries
				String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																	  OSLCConstants.OSLC_CM_V2,
																	  OSLCConstants.CM_CHANGE_REQUEST_TYPE);
				
				//SCENARIO A: Run a query for all open ChangeRequests with OSLC paging of 10 items per
				//page turned on and list the members of the result
				OslcQueryParameters queryParams = new OslcQueryParameters();
				queryParams.setWhere("oslc_cm:closed=false");
                queryParams.setSelect("dcterms:identifier,dcterms:title,oslc_cm:status");
                OslcQuery query = new OslcQuery(client, queryCapability, 10, queryParams);
				
				OslcQueryResult result = query.submit();
				
				boolean processAsJavaObjects = true;
				processPagedQueryResults(result,client, processAsJavaObjects);
				
				System.out.println("\n------------------------------\n");
				
				//SCENARIO B:  Run a query for a specific ChangeRequest selecting only certain 
				//attributes and then print it as raw XML.  Change the dcterms:identifier below to match a 
				//real workitem in your RTC project area
				OslcQueryParameters queryParams2 = new OslcQueryParameters();
				queryParams2.setWhere("dcterms:identifier=\"10\"");
				queryParams2.setSelect("dcterms:identifier,dcterms:title,dcterms:creator,dcterms:created,oslc_cm:status");
				OslcQuery query2 = new OslcQuery(client, queryCapability, queryParams2);
				
				OslcQueryResult result2 = query2.submit();
				ClientResponse rawResponse = result2.getRawResponse();
				processRawResponse(rawResponse);
				rawResponse.consumeContent();
				
				//SCENARIO C:  RTC Workitem creation and update
				ChangeRequest changeRequest = new ChangeRequest();
				changeRequest.setTitle("Implement accessibility in Pet Store application");
				changeRequest.setDescription("Image elements must provide a description in the 'alt' attribute for consumption by screen readers.");
				changeRequest.addTestedByTestCase(new Link(new URI("http://qmprovider/testcase/1"), "Accessibility verification using a screen reader"));
				changeRequest.addDctermsType("task");
				
				//Get the Creation Factory URL for change requests so that we can create one
				String changeRequestCreation = client.lookupCreationFactory(
						serviceProviderUrl, OSLCConstants.OSLC_CM_V2,
						changeRequest.getRdfTypes()[0].toString());

				//Create the change request
				ClientResponse creationResponse = client.createResource(
						changeRequestCreation, changeRequest,
						OslcMediaType.APPLICATION_RDF_XML,
						OslcMediaType.APPLICATION_RDF_XML);
				String changeRequestLocation = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);
				creationResponse.consumeContent();
				System.out.println("Change Request created a location " + changeRequestLocation);
				
				
				//Get the change request from the service provider and update its title property 
				changeRequest = client.getResource(changeRequestLocation,
						OslcMediaType.APPLICATION_RDF_XML).getEntity(
								ChangeRequest.class);
				changeRequest.setTitle(changeRequest.getTitle() + " (updated)");

				//Create a partial update URL so that only the title will be updated.
				//Assuming (for readability) that the change request URL does not already contain a '?'
				String updateUrl = changeRequest.getAbout() + "?oslc.properties=dcterms:title";
				
				//Update the change request at the service provider
				ClientResponse updateResponse = client.updateResource(
						updateUrl, changeRequest,
						OslcMediaType.APPLICATION_RDF_XML,
						OslcMediaType.APPLICATION_RDF_XML);
				
				updateResponse.consumeContent();
							
			}
		} catch (RootServicesException re) {
			logger.log(Level.SEVERE,"Unable to access the Jazz rootservices document at: " + webContextUrl + "/rootservices", re);
		} catch (Exception e) {
			logger.log(Level.SEVERE,e.getMessage(),e);
		}
		


	}
	
	private static void processPagedQueryResults(OslcQueryResult result, OslcClient client, boolean asJavaObjects) {
		int page = 1;
		do {
			System.out.println("\nPage " + page + ":\n");
			processCurrentPage(result,client,asJavaObjects);
			if (result.hasNext()) {
				result = result.next();
				page++;
			} else {
				break;
			}
		} while(true);
	}
	
	private static void processCurrentPage(OslcQueryResult result, OslcClient client, boolean asJavaObjects) {
		
	    for (ChangeRequest cr : result.getMembers(ChangeRequest.class)) {
            System.out.println("id: " + cr.getIdentifier() + ", title: " + cr.getTitle() + ", status: " + cr.getStatus());           
		}		
	}
	
	private static void processRawResponse(ClientResponse response) throws IOException {
		InputStream is = response.getEntity(InputStream.class);
		BufferedReader in = new BufferedReader(new InputStreamReader(is));
		
		String line = null;
		while((line = in.readLine()) != null) {
		  System.out.println(line);
		}
		System.out.println();
		response.consumeContent();
	}
	
	private static boolean validateOptions(CommandLine cmd) {
		boolean isValid = true;
		
		if (! (cmd.hasOption("url") &&
			   cmd.hasOption("user") &&
			   cmd.hasOption("password") &&
			   cmd.hasOption("project"))) {
			  
			isValid = false;
		}
		return isValid;		
	}

}
