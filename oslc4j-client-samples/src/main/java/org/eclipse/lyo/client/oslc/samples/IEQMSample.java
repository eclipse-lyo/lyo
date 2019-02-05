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

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.Response;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.ssl.SSLContextBuilder;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.client.oslc.JEEFormAuthenticator;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.eclipse.lyo.client.oslc.resources.TestCase;
import org.eclipse.lyo.client.oslc.resources.TestResult;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.glassfish.jersey.client.ClientConfig;

/**
 * Samples of logging in to Rational Quality Manager and running OSLC operations
 *
 * - run an OLSC TestResult query and retrieve OSLC TestResults and de-serialize them as Java objects
 * - retrieve an OSLC TestResult and print it as XML
 * - create a new TestCase
 * - update an existing TestCase
 *
 */
public class IEQMSample {

	private static final Logger logger = Logger.getLogger(IEQMSample.class.getName());

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
		String userId = cmd.getOptionValue("user");
		String password = cmd.getOptionValue("password");
		String projectArea = cmd.getOptionValue("project");

		try {

			// STEP 1: Configure the ClientBuilder as needed for your client application
			
			// Use HttpClient instead of the default HttpUrlConnection
			ClientConfig clientConfig = new ClientConfig().connectorProvider(new ApacheConnectorProvider());
			ClientBuilder clientBuilder = ClientBuilder.newBuilder();
			clientBuilder.withConfig(clientConfig);
			
			// Setup SSL support to ignore self-assigned SSL certificates - for testing only!!
		    SSLContextBuilder sslContextBuilder = new SSLContextBuilder();
		    sslContextBuilder.loadTrustMaterial(TrustSelfSignedStrategy.INSTANCE);
		    clientBuilder.sslContext(sslContextBuilder.build());
		    clientBuilder.hostnameVerifier(NoopHostnameVerifier.INSTANCE);
		    		    
		    // IBM jazz-apps use JEE Form based authentication
		    clientBuilder.register(new JEEFormAuthenticator(webContextUrl, userId, password));

		    //STEP 3: Create a new OslcClient
			OslcClient client = new OslcClient(clientBuilder);

			//STEP 4: Get the URL of the OSLC ChangeManagement service from the rootservices document
			String catalogUrl = client.getCatalogUrl(webContextUrl, OSLCConstants.OSLC_QM_V2);

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
			Response rawResponse = result2.getRawResponse();
			processRawResponse(rawResponse);

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
			Response creationResponse = client.createResource(
					testcaseCreation, testcase,
					OslcMediaType.APPLICATION_RDF_XML);
			if (creationResponse.getStatus() != HttpStatus.SC_CREATED) {
				System.err.println("ERROR: Could not create the test case (status " + creationResponse.getStatus() + ")\n");
				System.err.println(creationResponse.readEntity(String.class));
				System.exit(1);
			}

			creationResponse.readEntity(String.class);
			String testcaseLocation = creationResponse.getStringHeaders().getFirst(HttpHeaders.LOCATION);
			System.out.println("Test Case created at location " + testcaseLocation);

			//Get the test case from the service provider and update its title property
			testcase = client.getResource(testcaseLocation,
					OslcMediaType.APPLICATION_RDF_XML).readEntity(TestCase.class);
			testcase.setTitle(testcase.getTitle() + " (updated)");

			//Create a partial update URL so that only the title will be updated.
			//Assuming (for readability) that the test case URL does not already contain a '?'
			String updateUrl = testcase.getAbout() + "?oslc.properties=dcterms:title";

			//Update the test case at the service provider
			client.updateResource(updateUrl, testcase,
					OslcMediaType.APPLICATION_RDF_XML).readEntity(String.class);

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

			Response response = null;
			try {

				//Get a single artifact by its URL
				response = client.getResource(resultsUrl, OSLCConstants.CT_RDF);

				if (response != null) {
					//De-serialize it as a Java object
					if (asJavaObjects) {
						   TestResult tr = response.readEntity(TestResult.class);
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

	private static void processRawResponse(Response response) throws IOException {
		InputStream is = response.readEntity(InputStream.class);
		BufferedReader in = new BufferedReader(new InputStreamReader(is));

		String line = null;
		while((line = in.readLine()) != null) {
		  System.out.println(line);
		}
		System.out.println();
		response.readEntity(String.class);
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
