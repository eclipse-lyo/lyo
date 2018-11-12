/*******************************************************************************
 * Copyright (c) 2012, 2014 IBM Corporation.
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
 *     Samuel Padgett      - handle test case creation errors
 *     Samuel Padgett      - update command line example to use correct context
 *     Samuel Padgett      - set member property for RQM query results
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.logging.Level;
import java.util.logging.Logger;


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
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.eclipse.lyo.client.oslc.resources.TestCase;
import org.eclipse.lyo.client.oslc.resources.TestResult;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * Samples of logging in to Rational Quality Manager and running OSLC operations
 *
 * - run an OLSC TestResult query and retrieve OSLC TestResults and de-serialize them as Java objects
 * - retrieve an OSLC TestResult and print it as XML
 * - create a new TestCase
 * - update an existing TestCase
 *
 */
public class RQMFormSample {

	private static final Logger logger = Logger.getLogger(RQMFormSample.class.getName());

	/**
	 * Login to the RQM server and perform some OSLC actions
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
			logger.severe("Example: java RQMFormSample -url https://exmple.com:9443/qm -user ADMIN -password ADMIN -project \"JKE Banking (Quality Management)\"");
			return;
		}

		String webContextUrl = cmd.getOptionValue("url");
		String user = cmd.getOptionValue("user");
		String passwd = cmd.getOptionValue("password");
		String projectArea = cmd.getOptionValue("project");

		try {

			//STEP 1: Initialize a Jazz rootservices helper and indicate we're looking for the QualityManagement catalog
			//RQM contains both Quality and Change Management providers, so need to look for QM specifically
			JazzRootServicesHelper helper = new JazzRootServicesHelper(webContextUrl,OSLCConstants.OSLC_QM_V2);

			//STEP 2: Create a new Form Auth client with the supplied user/password
			JazzFormAuthClient client = helper.initFormClient(user, passwd);

			//STEP 3: Login in to Jazz Server
			if (client.login() == HttpStatus.SC_OK) {

				//STEP 4: Get the URL of the OSLC QualityManagement catalog
				String catalogUrl = helper.getCatalogUrl();

				//STEP 5: Find the OSLC Service Provider for the project area we want to work with
				String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, projectArea);

				//STEP 6: Get the Query Capabilities URL so that we can run some OSLC queries
				String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																	  OSLCConstants.OSLC_QM_V2,
																	  OSLCConstants.QM_TEST_RESULT_QUERY);

				//SCENARIO A: Run a query for all TestResults with a status of passed with OSLC paging of 10 items per
				//page turned on and list the members of the result
				OslcQueryParameters queryParams = new OslcQueryParameters();
				queryParams.setWhere("oslc_qm:status=\"com.ibm.rqm.execution.common.state.passed\"");
				OslcQuery query = new OslcQuery(client, queryCapability, 10, queryParams);

				System.out.println("Running query: " + query.getQueryUrl());
				OslcQueryResult result = query.submit();
				result.setMemberProperty(OSLCConstants.OSLC_QM_V2 + "testResult");

				boolean processAsJavaObjects = true;
				processPagedQueryResults(result,client, processAsJavaObjects);

				System.out.println("\n------------------------------\n");

				//SCENARIO B:  Run a query for a specific TestResult selecting only certain
				//attributes and then print it as raw XML.  Change the dcterms:title below to match a
				//real TestResult in your RQM project area
				OslcQueryParameters queryParams2 = new OslcQueryParameters();
				queryParams2.setWhere("dcterms:title=\"Consistent_display_of_currency_Firefox_DB2_WAS_Windows_S1\"");
				queryParams2.setSelect("dcterms:identifier,dcterms:title,dcterms:creator,dcterms:created,oslc_qm:status");
				OslcQuery query2 = new OslcQuery(client, queryCapability, queryParams2);

				OslcQueryResult result2 = query2.submit();
				result2.setMemberProperty(OSLCConstants.OSLC_QM_V2 + "testResult");
				ClientResponse rawResponse = result2.getRawResponse();
				processRawResponse(rawResponse);
				rawResponse.consumeContent();

				//SCENARIO C:  RQM TestCase creation and update
				TestCase testcase = new TestCase();
				testcase.setTitle("Accessibility verification using a screen reader");
				testcase.setDescription("This test case uses a screen reader application to ensure that the web browser content fully complies with accessibility standards");
				testcase.addTestsChangeRequest(new Link(new URI("http://cmprovider/changerequest/1"), "Implement accessibility in Pet Store application"));

				//Get the Creation Factory URL for test cases so that we can create a test case
				String testcaseCreation = client.lookupCreationFactory(
						serviceProviderUrl, OSLCConstants.OSLC_QM_V2,
						testcase.getRdfTypes()[0].toString());

				//Create the test case
				ClientResponse creationResponse = client.createResource(
						testcaseCreation, testcase,
						OslcMediaType.APPLICATION_RDF_XML);
				if (creationResponse.getStatusCode() != HttpStatus.SC_CREATED) {
					System.err.println("ERROR: Could not create the test case (status " + creationResponse.getStatusCode() + ")\n");
					System.err.println(creationResponse.getEntity(String.class));
					System.exit(1);
				}

				creationResponse.consumeContent();
				String testcaseLocation = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);
				System.out.println("Test Case created at location " + testcaseLocation);

				//Get the test case from the service provider and update its title property
				testcase = client.getResource(testcaseLocation,
						OslcMediaType.APPLICATION_RDF_XML).getEntity(
						TestCase.class);
				testcase.setTitle(testcase.getTitle() + " (updated)");

				//Create a partial update URL so that only the title will be updated.
				//Assuming (for readability) that the test case URL does not already contain a '?'
				String updateUrl = testcase.getAbout() + "?oslc.properties=dcterms:title";

				//Update the test case at the service provider
				client.updateResource(updateUrl, testcase,
						OslcMediaType.APPLICATION_RDF_XML).consumeContent();


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

		for (String resultsUrl : result.getMembersUrls()) {
			System.out.println(resultsUrl);

			ClientResponse response = null;
			try {

				//Get a single artifact by its URL
				response = client.getResource(resultsUrl, OSLCConstants.CT_RDF);

				if (response != null) {
					//De-serialize it as a Java object
					if (asJavaObjects) {
						   TestResult tr = response.getEntity(TestResult.class);
						   printTestResultInfo(tr);   //print a few attributes
					} else {

						//Just print the raw RDF/XML (or process the XML as desired)
						processRawResponse(response);

					}
				}
			} catch (Exception e) {
				logger.log(Level.SEVERE, "Unable to process artifact at url: " + resultsUrl, e);
			}

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

	private static void printTestResultInfo(TestResult tr) {
		//See the OSLC4J TestResult class for a full list of attributes you can access.
		if (tr != null) {
			System.out.println("ID: " + tr.getIdentifier() + ", Title: " + tr.getTitle() + ", Status: " + tr.getStatus());
		}
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
