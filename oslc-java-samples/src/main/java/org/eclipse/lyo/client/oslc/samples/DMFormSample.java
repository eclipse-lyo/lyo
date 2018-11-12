/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.http.HttpStatus;
import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.jazz.JazzFormAuthClient;
import org.eclipse.lyo.client.oslc.jazz.JazzRootServicesHelper;
import org.eclipse.lyo.client.oslc.resources.ArchitectureResource;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;

/**
 * Samples of logging in to Rational Design Manager and running OSLC operations
 *
 *
 * - run an OLSC Architecture Management Resource query and retrieve OSLC AM Resources and de-serialize them as Java objects
 *
 */
public class DMFormSample {

	private static final Logger logger = Logger.getLogger(DMFormSample.class.getName());

	/**
	 * Login to the DM server and perform some OSLC actions
	 * @param args
	 * @throws ParseException
	 */
	public static void main(String[] args) throws ParseException {

		Options options=new Options();

		options.addOption("url", true, "url");
		options.addOption("user", true, "user ID");
		options.addOption("password", true, "password");
		options.addOption("project",true,"project area");
		options.addOption("raw", false, "print raw object");  // useful for custom domains

		CommandLineParser cliParser = new GnuParser();

		//Parse the command line
		CommandLine cmd = cliParser.parse(options, args);

		if (!validateOptions(cmd)) {
			logger.severe("Syntax:  java <class_name> -url https://<server>:port/<context>/ -user <user> -password <password> -project \"<project_area>\"");
			logger.severe("Example: java DMFormSample -url https://exmple.com:9443/dm -user ADMIN -password ADMIN -project \"JKE Banking (Design Management)\"");
			return;
		}

		String webContextUrl = cmd.getOptionValue("url");
		String user = cmd.getOptionValue("user");
		String passwd = cmd.getOptionValue("password");
		String projectArea = cmd.getOptionValue("project");
		Boolean processAsJavaObjects = !cmd.hasOption("raw");

		try {

			//STEP 1: Initialize a Jazz rootservices helper and indicate we're looking for the ArchitectureManagement catalog
			JazzRootServicesHelper helper = new JazzRootServicesHelper(webContextUrl,OSLCConstants.OSLC_AM_V2);

			//STEP 2: Create a new Form Auth client with the supplied user/password
			//RRC is a fronting server, so need to use the initForm() signature which allows passing of an authentication URL.
			//For RRC, use the JTS for the authorization URL

			//Set the URL to access to authorize the user.  For DM, this is the JTS context.
			//This is a bit of a hack for readability.  It is assuming DM is at context /dm.  Could use a regex or UriBuilder instead.
			String authUrl = webContextUrl.replaceFirst("/dm","/jts");
			JazzFormAuthClient client = helper.initFormClient(user, passwd, authUrl);

			//STEP 3: Login in to Jazz Server
			if (client.login() == HttpStatus.SC_OK) {

				//STEP 4: Get the URL of the OSLC ChangeManagement catalog
				String catalogUrl = helper.getCatalogUrl();

				//STEP 5: Find the OSLC Service Provider for the project area we want to work with
				String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, projectArea);

				//STEP 6: Get the Query Capabilities URL so that we can run some OSLC queries
				String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																	  OSLCConstants.OSLC_AM_V2,
																	  OSLCConstants.AM_RESOURCE_TYPE);



				//SCENARIO A: Run a query for all AM Resources with OSLC paging of 10 items per
				//page turned on and list the members of the result

				OslcQueryParameters queryParams = new OslcQueryParameters();

				OslcQuery query = new OslcQuery(client, queryCapability, 10, queryParams);

				OslcQueryResult result = query.submit();

				processPagedQueryResults(result,client, processAsJavaObjects);

				System.out.println("\n------------------------------\n");

				//TODO:  Add more DM scenarios
			}
		} catch (RootServicesException re) {
			logger.log(Level.SEVERE,"Unable to access the Jazz rootservices document at: " + webContextUrl + "/rootservices", re);
		} catch (Exception e) {
			logger.log(Level.SEVERE,e.getMessage(),e);
		}



	}

	private static void processPagedQueryResults(OslcQueryResult result, OslcClient client, boolean asJavaObjects) {
		int page = 1;
		//For now, just show first 5 pages
		do {
			System.out.println("\nPage " + page + ":\n");
			processCurrentPage(result,client,asJavaObjects);
			if (result.hasNext() && page < 5) {
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
						   ArchitectureResource resource = response.getEntity(ArchitectureResource.class);
						   printResourceInfo(resource);   //print a few attributes
					} else {

						//Just print the raw RDF/XML (or process the XML as desired)
						processRawResponse(response);

					}
				}
			} catch (java.lang.reflect.UndeclaredThrowableException e) {
				// printing the exception causes an infinite loop if the resource is a folder, not oslc_am:Resource
				logger.log(Level.SEVERE, "Unable to process artfiact at url: " + resultsUrl);
			} catch (Exception e) {
				logger.log(Level.SEVERE, "Unable to process artfiact at url: " + resultsUrl, e);
			}

		}

	}

	private static void printResourceInfo(ArchitectureResource resource) {
		if (resource != null) {
			System.out.println("ID: " + resource.getIdentifier() + " Title: " + resource.getTitle());
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
