/*!*****************************************************************************
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
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.apache.http.HttpStatus;
import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.ChangeRequest;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Samples of accessing a generic ChangeManagement provider and running OSLC operations.
 * 
 * This will not run against any CM server that requires authentication. Use with the
 * eclipse/Lyo sample CM servers.
 *
 *
 * - run an OLSC ChangeRequest query and retrieve OSLC ChangeRequests and de-serialize them as Java objects
 * - retrieve an OSLC ChangeRequest and print it as XML
 * - create a new ChangeRequest
 * - update an existing ChangeRequest
 *
 */
public class GenericCMSample {

	private final static Logger logger = LoggerFactory.getLogger(GenericCMSample.class);

	/**
	 * Access a CM service provider and perform some OSLC actions
	 * @param args
	 * @throws ParseException
	 */
	public static void main(String[] args) throws ParseException {

		Options options=new Options();

		options.addOption("url", true, "url");  //the OSLC catalog URL
		options.addOption("providerTitle", true, "Service Provider title");

		CommandLineParser cliParser = new GnuParser();

		//Parse the command line
		CommandLine cmd = cliParser.parse(options, args);

		if (!validateOptions(cmd)) {
			logger.error("Syntax:  java <class_name> -url https://<server>:port/<context>/<catalog_location> -providerTitle \"<provider title>\"");
			logger.error("Example: java GenericCMSample -url https://exmple.com:8080/OSLC4JRegistry/catalog/1 -providerTitle \"OSLC Lyo Change Management Service Provider\"");
			return;
		}

		String catalogUrl = cmd.getOptionValue("url");
		String providerTitle = cmd.getOptionValue("providerTitle");

		try {

			logger.info("Initialising OSLC Client");
			//STEP 1: Create a new generic OslcClient
			OslcClient client = new OslcClient();

			logger.info("Looking up Service Provider by URI={} and title='{}'", catalogUrl, providerTitle);
			//STEP 2: Find the OSLC Service Provider for the service provider we want to work with
			String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, providerTitle);

			//STEP 3: Get the Query Capabilities and Creation Factory URLs so that we can run some OSLC queries
			String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																  OSLCConstants.OSLC_CM_V2,
																  OSLCConstants.CM_CHANGE_REQUEST_TYPE);

			String creationFactory = client.lookupCreationFactory(serviceProviderUrl,
					  											  OSLCConstants.OSLC_CM_V2,
					  											  OSLCConstants.CM_CHANGE_REQUEST_TYPE);

			//SCENARIO A: Run a query for all ChangeRequests

			OslcQueryParameters queryParams = new OslcQueryParameters();
			OslcQuery query = new OslcQuery(client, queryCapability);

			OslcQueryResult result = query.submit();

			boolean processAsJavaObjects = true;
			processPagedQueryResults(result,client, processAsJavaObjects);

			System.out.println("\n------------------------------\n");

			//SCENARIO B:  Run a query for a specific ChangeRequest and then print it as raw XML.
			//Change the URL below to match a real ChangeRequest

			ClientResponse rawResponse = client.getResource("http://localhost:8080/OSLC4JChangeManagement/services/changeRequests/1",
									 					     OSLCConstants.CT_XML);
			processRawResponse(rawResponse);
			rawResponse.consumeContent();

			//SCENARIO C:  ChangeRequest creation and update
			ChangeRequest newChangeRequest = new ChangeRequest();
			newChangeRequest.setTitle("Update database schema");
			newChangeRequest.setTitle("Need to update the database schema to reflect the data model changes");

			rawResponse = client.createResource(creationFactory, newChangeRequest, OSLCConstants.CT_RDF);
			int statusCode = rawResponse.getStatusCode();
			rawResponse.consumeContent();
			System.out.println("Status code for POST of new artifact: " + statusCode);

			if (statusCode == HttpStatus.SC_CREATED) {
				String location = rawResponse.getHeaders().getFirst("Location");
				newChangeRequest.setClosed(false);
				newChangeRequest.setInProgress(true);
				rawResponse = client.updateResource(location, newChangeRequest, OSLCConstants.CT_RDF);
				rawResponse.consumeContent();
				System.out.println("Status code for PUT of updated artifact: " + rawResponse.getStatusCode());
			}



		} catch (Exception e) {
			logger.error("Unknown error", e);
		}
	}

	private static void processPagedQueryResults(OslcQueryResult result, OslcClient client, boolean asJavaObjects) {
		int page = 1;
		do {
			System.out.println("\nPage " + page + ":\n");
			final int statusCode = result.getRawResponse().getStatusCode();
			if(statusCode >= 400) {
				logger.error("Query failed: {} on page {}", statusCode, result.getQuery().getQueryUrl());
				return;
			}
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
						   ChangeRequest cr = response.getEntity(ChangeRequest.class);
						   printChangeRequestInfo(cr);   //print a few attributes
					} else {

						//Just print the raw RDF/XML (or process the XML as desired)
						processRawResponse(response);

					}
				}
			} catch (Exception e) {
				logger.error("Unable to process artifact at url: {}", resultsUrl, e);
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
	}

	private static void printChangeRequestInfo(ChangeRequest cr) {
		//See the OSLC4J ChangeRequest class for a full list of attributes you can access.
		if (cr != null) {
			System.out.println("ID: " + cr.getIdentifier() + ", Title: " + cr.getTitle() + ", Status: " + cr.getStatus());
		}
	}

	private static boolean validateOptions(CommandLine cmd) {
		boolean isValid = true;

		if (! (cmd.hasOption("url") &&
			  (cmd.hasOption("providerTitle")))) {

			isValid = false;
		}
		return isValid;
	}

}
