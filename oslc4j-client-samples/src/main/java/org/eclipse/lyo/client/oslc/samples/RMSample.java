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
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.http.HttpStatus;
import javax.ws.rs.core.Response;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.Requirement;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.eclipse.lyo.client.oslc.resources.Requirement;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.ParseException;

/**
 * Samples of accessing a generic Requirements Management provider and running OSLC operations.
 * 
 * This will not run against any RM server that doesn't require authentication. Use with the
 * eclipse/Lyo sample RM servers.
 *
 *
 * - run an OLSC Requirement query and retrieve OSLC Requirements and de-serialize them as Java objects
 * - retrieve an OSLC Requirement and print it as XML
 * - create a new Requirement
 * - update an existing Requirement
 *
 */
public class RMSample {

	private static final Logger logger = Logger.getLogger(RMSample.class.getName());

	/**
	 * Access a RM service provider and perform some OSLC actions
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
			logger.severe("Syntax:  java <class_name> -url https://<server>:port/<context>/<catalog_location> -providerTitle \"<provider title>\"");
			logger.severe("Example: java GenericCMSample -url https://localhost:8080/OSLC4JRegistry/catalog/singleton -providerTitle \"OSLC Lyo Requirements Management Service Provider\"");
			return;
		}

		String catalogUrl = cmd.getOptionValue("url");
		String providerTitle = cmd.getOptionValue("providerTitle");

		try {

			//STEP 1: Create a new generic OslcClient
			OslcClient client = new OslcClient();

			//STEP 2: Find the OSLC Service Provider for the service provider we want to work with
			String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, providerTitle);

			//STEP 3: Get the Query Capabilities and Creation Factory URLs so that we can run some OSLC queries
			String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																  OSLCConstants.OSLC_RM_V2,
																  OSLCConstants.RM_REQUIREMENT_TYPE);

			String creationFactory = client.lookupCreationFactory(serviceProviderUrl,
					  OSLCConstants.OSLC_RM_V2,
					  OSLCConstants.RM_REQUIREMENT_TYPE);


			//SCENARIO A: Run a query for all Requirements

			OslcQueryParameters queryParams = new OslcQueryParameters();
			OslcQuery query = new OslcQuery(client, queryCapability);

			OslcQueryResult result = query.submit();

			boolean processAsJavaObjects = true;
			processPagedQueryResults(result,client, processAsJavaObjects);

			System.out.println("\n------------------------------\n");

			//SCENARIO B:  Run a query for a specific Requirement and then print it as raw XML.
			//Change the URL below to match a real Requirement

			Response rawResponse = client.getResource("http://localhost:8086/adaptor-rm/services/requirements/3513",
									 					     OSLCConstants.CT_XML);
			processRawResponse(rawResponse);
			rawResponse.readEntity(String.class);

			//SCENARIO C:  Requirement creation and update
			Requirement newRequirement = new Requirement();
			newRequirement.setTitle("Database schema needs new attributes");
			newRequirement.setTitle("The data model needs to support new attributes");

			rawResponse = client.createResource(creationFactory, newRequirement, OSLCConstants.CT_RDF);
			int statusCode = rawResponse.getStatus();
			rawResponse.readEntity(String.class);
			System.out.println("Status code for POST of new artifact: " + statusCode);

			if (statusCode == HttpStatus.SC_CREATED) {
				String location = rawResponse.getStringHeaders().getFirst("Location");
				newRequirement.setTitle("The schema needs to support new attributes");
				rawResponse = client.updateResource(location, newRequirement, OSLCConstants.CT_RDF);
				rawResponse.readEntity(String.class);
				System.out.println("Status code for PUT of updated artifact: " + rawResponse.getStatus());
			}



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
						Requirement cr = response.readEntity(Requirement.class);
						   printRequirementInfo(cr);   //print a few attributes
					} else {

						//Just print the raw RDF/XML (or process the XML as desired)
						processRawResponse(response);

					}
				}
			} catch (Exception e) {
				logger.log(Level.SEVERE, "Unable to process artfiact at url: " + resultsUrl, e);
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
	}

	private static void printRequirementInfo(Requirement cr) {
		//See the OSLC4J Requirement class for a full list of attributes you can access.
		if (cr != null) {
			System.out.println("ID: " + cr.getIdentifier() + ", Title: " + cr.getTitle() + ", Description: " + cr.getDescription());
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
