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

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.Response;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.http.HttpStatus;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.ssl.SSLContextBuilder;
import org.eclipse.lyo.client.oslc.JEEFormAuthenticator;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.resources.ChangeRequest;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;

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
public class CMSample {

	private static final Logger logger = Logger.getLogger(CMSample.class.getName());

	/**
	 * Access a CM server and perform some OSLC actions.
	 * @param args
	 * @throws ParseException
	 */
	public static void main(String[] args) throws ParseException {

		Options options=new Options();

		options.addOption("catalogURL", true, "OSLC ServiceProviderCatalog URL");
		options.addOption("providerTitle", true, "Service Provider title in the ServiceProviderCatalog");
		options.addOption("user", true, "User ID");
		options.addOption("password", true, "User's password");

		CommandLineParser cliParser = new GnuParser();

		//Parse the command line
		CommandLine cmd = cliParser.parse(options, args);

		if (!validateOptions(cmd)) {
			logger.severe("Syntax:  java <class_name> -catalogURL https://<server>:port/<context>/<catalog_location> -providerTitle \"<provider title>\" -user userID -password password");
			logger.severe("Example: java GenericCMSample -catalogURL https://exmple.com:8080/OSLC4JRegistry/catalog/1 -providerTitle \"OSLC Lyo Change Management Service Provider\" -user fred -password pasw0rd");
			return;
		}

		// for jazz.net apps, this is the baseUri for the server, e.g., https://ce4iot.rtp.raleigh.ibm.com:9443/ccm
		String catalogUrl = cmd.getOptionValue("catalogURL");
		String providerTitle = cmd.getOptionValue("providerTitle");
		String userId = cmd.getOptionValue("user");
		String password = cmd.getOptionValue("password");

		try {

			// STEP 0: Configure the ClientBuilder as needed for your client application
			
			// Use HttpClient instead of the default HttpUrlConnection
			ClientConfig clientConfig = new ClientConfig().connectorProvider(new ApacheConnectorProvider());
			ClientBuilder clientBuilder = ClientBuilder.newBuilder();
			clientBuilder.withConfig(clientConfig);
			
			// Setup SSL support to ignore self-assigned SSL certificates - for testing only!!
		    SSLContextBuilder sslContextBuilder = new SSLContextBuilder();
		    sslContextBuilder.loadTrustMaterial(TrustSelfSignedStrategy.INSTANCE);
		    clientBuilder.sslContext(sslContextBuilder.build());
		    clientBuilder.hostnameVerifier(NoopHostnameVerifier.INSTANCE);
		    
		    // Use preemptive Basic authentication
		    HttpAuthenticationFeature authFeature = HttpAuthenticationFeature.basic(userId, password);
		    clientBuilder.register(authFeature);
		    
		    //STEP 1: Create a new OslcClient
			OslcClient client = new OslcClient(clientBuilder);

			//STEP 2: Find the OSLC Service Provider for the service provider we want to work with
			String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, providerTitle);
			System.out.println("serviceProviderUrl: "+serviceProviderUrl);

			//STEP 3: Get the Query Capabilities and Creation Factory URLs so that we can run some OSLC queries
			String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																  OSLCConstants.OSLC_CM_V2,
																  OSLCConstants.CM_CHANGE_REQUEST_TYPE);
			System.out.println("queryCapability: "+queryCapability);

			String creationFactory = client.lookupCreationFactory(serviceProviderUrl,
					  											  OSLCConstants.OSLC_CM_V2,
					  											  OSLCConstants.CM_CHANGE_REQUEST_TYPE);
			System.out.println("creationFactory: "+creationFactory);

			//SCENARIO A: Run a query for all ChangeRequests

			OslcQueryParameters queryParams = new OslcQueryParameters();
			OslcQuery query = new OslcQuery(client, queryCapability);

			OslcQueryResult result = query.submit();

			boolean processAsJavaObjects = true;
			processPagedQueryResults(result, client, processAsJavaObjects);

			System.out.println("\n------------------------------\n");

			//SCENARIO B:  Run a query for a specific ChangeRequest and then print it as raw XML.
			//Change the URL below to match a real ChangeRequest

			Response rawResponse = client.getResource("http://localhost:8080/OSLC4JChangeManagement/services/changeRequests/2",
									 					     OSLCConstants.CT_XML);
			processRawResponse(rawResponse);
			rawResponse.readEntity(String.class);

			//SCENARIO C:  ChangeRequest creation and update
			ChangeRequest newChangeRequest = new ChangeRequest();
			newChangeRequest.setTitle("Update database schema");
			newChangeRequest.setTitle("Need to update the database schema to reflect the data model changes");

			rawResponse = client.createResource(creationFactory, newChangeRequest, OSLCConstants.CT_RDF);
			int statusCode = rawResponse.getStatus();
			rawResponse.readEntity(String.class);
			System.out.println("Status code for POST of new artifact: " + statusCode);

			if (statusCode == HttpStatus.SC_CREATED) {
				String location = rawResponse.getStringHeaders().getFirst("Location");
				newChangeRequest.setClosed(false);
				newChangeRequest.setInProgress(true);
				rawResponse = client.updateResource(location, newChangeRequest, OSLCConstants.CT_RDF);
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
						   ChangeRequest cr = response.readEntity(ChangeRequest.class);
						   printChangeRequestInfo(cr);   //print a few attributes
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

	private static void printChangeRequestInfo(ChangeRequest cr) {
		//See the OSLC4J ChangeRequest class for a full list of attributes you can access.
		if (cr != null) {
			System.out.println("ID: " + cr.getIdentifier() + ", Title: " + cr.getTitle() + ", Status: " + cr.getStatus());
		}
	}

	private static boolean validateOptions(CommandLine cmd) {
		boolean isValid = true;

		if (! (cmd.hasOption("catalogURL") &&
			  (cmd.hasOption("providerTitle")))) {

			isValid = false;
		}
		return isValid;
	}

}
