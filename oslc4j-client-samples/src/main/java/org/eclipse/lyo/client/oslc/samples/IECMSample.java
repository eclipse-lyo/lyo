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
 *     Steve Pitschke      - added use of new OslcQueryResult.getMembers()
 *     Samuel Padgett      - examine oslc:usage to find the right creation factory,
 *                           use resource shapes to discover allowed values for Filed Against
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
import javax.xml.namespace.QName;

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
import org.eclipse.lyo.client.oslc.resources.ChangeRequest;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.eclipse.lyo.oslc4j.core.model.AllowedValues;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.Property;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.provider.jena.AbstractOslcRdfXmlProvider;
import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.glassfish.jersey.client.ClientConfig;

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
public class IECMSample {

	private static final String RTC_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/rtc/cm/1.0/";
	private static final String RTC_FILED_AGAINST = "filedAgainst";
	private static final Logger logger = Logger.getLogger(IECMSample.class.getName());

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

		// RTC sometimes will declare a property's type, but leave the value
		// empty, which causes errors when parsed by OSLC4J. Set a system property
		// to tell OSLC4J to skip these invalid values.
		System.setProperty(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES, "false");

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
			String catalogUrl = client.getCatalogUrl(webContextUrl, OSLCConstants.OSLC_CM_V2);

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

			processPagedQueryResults(result, client);

			System.out.println("\n------------------------------\n");

			//SCENARIO B:  Run a query for a specific ChangeRequest selecting only certain
			//attributes and then print it as raw XML.  Change the dcterms:identifier below to match a
			//real workitem in your RTC project area
			OslcQueryParameters queryParams2 = new OslcQueryParameters();
			queryParams2.setWhere("dcterms:identifier=7");
			queryParams2.setSelect("dcterms:identifier,dcterms:title,dcterms:creator,dcterms:created,oslc_cm:status");
			OslcQuery query2 = new OslcQuery(client, queryCapability, queryParams2);

			OslcQueryResult result2 = query2.submit();
			Response rawResponse = result2.getRawResponse();
			processRawResponse(rawResponse);
			rawResponse.close();

			//SCENARIO C:  RTC task creation and update
			ChangeRequest task = new ChangeRequest();
			task.setTitle("Implement accessibility in Pet Store application");
			task.setDescription("Image elements must provide a description in the 'alt' attribute for consumption by screen readers.");
			task.addTestedByTestCase(new Link(new URI("http://qmprovider/testcase/1"), "Accessibility verification using a screen reader"));
			task.addDctermsType("task");
			
			//Get the Creation Factory URL for task change requests so that we can create one
			CreationFactory taskCreation = client.lookupCreationFactoryResource(
					serviceProviderUrl, OSLCConstants.OSLC_CM_V2,
					task.getRdfTypes()[0].toString(), OSLCConstants.OSLC_CM_V2 + "task");
			String factoryUrl = taskCreation.getCreation().toString();

			//Determine what to use for the Filed Against attribute by requesting the resource shape for the creation factory.
			String shapeUrl = taskCreation.getResourceShapes()[0].toString();
			Response shapeResponse = client.getResource(shapeUrl);
			ResourceShape shape = shapeResponse.readEntity(ResourceShape.class);

			//Look at the allowed values for Filed Against. This is generally a required field for defects.
			Property filedAgainstProperty = shape.getProperty(new URI(RTC_NAMESPACE + RTC_FILED_AGAINST));
			if (filedAgainstProperty != null) {
				URI allowedValuesRef = filedAgainstProperty.getAllowedValuesRef();
				Response allowedValuesResponse = client.getResource(allowedValuesRef.toString());
				AllowedValues allowedValues = allowedValuesResponse.readEntity(AllowedValues.class);
				Object[] values = allowedValues.getValues().toArray();
				task.getExtendedProperties().put(new QName(RTC_NAMESPACE, RTC_FILED_AGAINST), (URI) values[0]);
			}

			//Create the change request
			Response creationResponse = client.createResource(
					factoryUrl, task,
					OslcMediaType.APPLICATION_RDF_XML,
					OslcMediaType.APPLICATION_RDF_XML);
			String changeRequestLocation = creationResponse.getStringHeaders().getFirst(HttpHeaders.LOCATION);
			if (creationResponse.getStatus() != HttpStatus.SC_CREATED) {
				System.err.println("ERROR: Could not create the task (status " + creationResponse.getStatus() + ")\n");
				System.err.println(creationResponse.readEntity(String.class));
				System.exit(1);
			}
			creationResponse.readEntity(String.class);
			System.out.println("Task created at location " + changeRequestLocation);

			//Get the change request from the service provider and update its title property
			task = client.getResource(changeRequestLocation).readEntity(ChangeRequest.class);
			task.setTitle(task.getTitle() + " (updated)");

			//Create a partial update URL so that only the title will be updated.
			//Assuming (for readability) that the change request URL does not already contain a '?'
			String updateUrl = task.getAbout() + "?oslc.properties=dcterms:title";

			//Update the change request at the service provider
			Response updateResponse = client.updateResource(
					updateUrl, task,
					OslcMediaType.APPLICATION_RDF_XML,
					OslcMediaType.APPLICATION_RDF_XML);

			updateResponse.readEntity(String.class);

			//SCENARIO D:  RTC defect creation
			ChangeRequest defect = new ChangeRequest();
			defect.setTitle("Error logging in");
			defect.setDescription("An error occurred when I tried to log in with a user ID that contained the '@' symbol.");
			defect.addTestedByTestCase(new Link(new URI("http://qmprovider/testcase/3"), "Global Verifcation Test"));
			defect.addDctermsType("defect");

			//Get the Creation Factory URL for change requests so that we can create one
			CreationFactory defectCreation = client.lookupCreationFactoryResource(
					serviceProviderUrl, OSLCConstants.OSLC_CM_V2,
					defect.getRdfTypes()[0].toString(), OSLCConstants.OSLC_CM_V2 + "defect");
			factoryUrl = defectCreation.getCreation().toString();

			//Determine what to use for the Filed Against attribute by requesting the resource shape for the creation factory.
			shapeUrl = defectCreation.getResourceShapes()[0].toString();
			shapeResponse = client.getResource(shapeUrl);
			shape = shapeResponse.readEntity(ResourceShape.class);

			//Look at the allowed values for Filed Against. This is generally a required field for defects.
			filedAgainstProperty = shape.getProperty(new URI(RTC_NAMESPACE + RTC_FILED_AGAINST));
			if (filedAgainstProperty != null) {
				URI allowedValuesRef = filedAgainstProperty.getAllowedValuesRef();
				Response allowedValuesResponse = client.getResource(allowedValuesRef.toString());
				AllowedValues allowedValues = allowedValuesResponse.readEntity(AllowedValues.class);
				Object[] values = allowedValues.getValues().toArray();
				//If this fails, you might need to check that the value is
				//not "Unassigned", which is an allowed value in some RTC
				//project areas. Try the second value instead of the first, most project area processes
				//create more than one category
				defect.getExtendedProperties().put(new QName(RTC_NAMESPACE, RTC_FILED_AGAINST), (URI) values[1]);
			}

			//Create the change request
			creationResponse = client.createResource(
					factoryUrl, defect,
					OslcMediaType.APPLICATION_RDF_XML,
					OslcMediaType.APPLICATION_RDF_XML);
			String defectLocation = creationResponse.getStringHeaders().getFirst(HttpHeaders.LOCATION);
			if (creationResponse.getStatus() != HttpStatus.SC_CREATED) {
				System.err.println("ERROR: Could not create the defect (status " + creationResponse.getStatus() + ")\n");
				System.err.println(creationResponse.readEntity(String.class));
				System.exit(1);
			}
			creationResponse.readEntity(String.class);
			System.out.println("Defect created at location " + defectLocation);
		} catch (RootServicesException re) {
			logger.log(Level.SEVERE,"Unable to access the Jazz rootservices document at: " + webContextUrl + "/rootservices", re);
		} catch (Exception e) {
			logger.log(Level.SEVERE,e.getMessage(),e);
		}
	}

	private static void processPagedQueryResults(OslcQueryResult result, OslcClient client) {
		int page = 1;
		do {
			System.out.println("\nPage " + page + ":\n");
			processCurrentPage(result, client);
			if (result.hasNext()) {
				result = result.next();
				page++;
			} else {
				break;
			}
		} while(true);
	}

	private static void processCurrentPage(OslcQueryResult result, OslcClient client) {

	    for (ChangeRequest cr : result.getMembers(ChangeRequest.class)) {
            System.out.println("id: " + cr.getIdentifier() + ", title: " + cr.getTitle() + ", status: " + cr.getStatus());
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
