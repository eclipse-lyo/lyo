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
 *     Gabriel Ruelas      - Fix handling of Rich text, include parsing extended properties
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

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
import org.eclipse.lyo.client.oslc.resources.Requirement;
import org.eclipse.lyo.client.oslc.resources.RequirementCollection;
import org.eclipse.lyo.client.oslc.resources.RmConstants;
import org.eclipse.lyo.client.oslc.resources.RmUtil;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.Property;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;




/**
 * Samples of logging in to Rational Requirements Composer or Rational DOORS Next Generation
 * and running OSLC operations
 *
 *
 * - run an OLSC Requirement query and retrieve OSLC Requirements and de-serialize them as Java objects
 * - TODO:  Add more requirement sample scenarios
 *
 */
public class RRCFormSample {

	private static final Logger logger = Logger.getLogger(RRCFormSample.class.getName());

	// Following is a workaround for primaryText issue in DNG ( it is PrimaryText instead of primaryText
	private static final QName PROPERTY_PRIMARY_TEXT_WORKAROUND   = new QName(RmConstants.JAZZ_RM_NAMESPACE, "PrimaryText");

	/**
	 * Login to the RRC server and perform some OSLC actions
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
			logger.severe("Example: java RRCFormSample -url https://exmple.com:9443/rm -user ADMIN -password ADMIN -project \"JKE Banking (Requirements Management)\"");
			return;
		}

		String webContextUrl = cmd.getOptionValue("url");
		String user = cmd.getOptionValue("user");
		String passwd = cmd.getOptionValue("password");
		String projectArea = cmd.getOptionValue("project");


		try {

			//STEP 1: Initialize a Jazz rootservices helper and indicate we're looking for the RequirementManagement catalog
			JazzRootServicesHelper helper = new JazzRootServicesHelper(webContextUrl,OSLCConstants.OSLC_RM_V2);

			//STEP 2: Create a new Form Auth client with the supplied user/password
			//RRC is a fronting server, so need to use the initForm() signature which allows passing of an authentication URL.
			//For RRC, use the JTS for the authorization URL

			//This is a bit of a hack for readability.  It is assuming RRC is at context /rm.  Could use a regex or UriBuilder instead.
			String authUrl = webContextUrl.replaceFirst("/rm","/jts");
			JazzFormAuthClient client = helper.initFormClient(user, passwd, authUrl);

			//STEP 3: Login in to Jazz Server
			if (client.login() == HttpStatus.SC_OK) {

				//STEP 4: Get the URL of the OSLC ChangeManagement catalog
				String catalogUrl = helper.getCatalogUrl();

				//STEP 5: Find the OSLC Service Provider for the project area we want to work with
				String serviceProviderUrl = client.lookupServiceProviderUrl(catalogUrl, projectArea);

				//STEP 6: Get the Query Capabilities URL so that we can run some OSLC queries
				String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																	  OSLCConstants.OSLC_RM_V2,
																	  OSLCConstants.RM_REQUIREMENT_TYPE);
				//STEP 7: Create base requirements
				//Get the Creation Factory URL for change requests so that we can create one

				String requirementFactory = client.lookupCreationFactory(
						serviceProviderUrl, OSLCConstants.OSLC_RM_V2,
						OSLCConstants.RM_REQUIREMENT_TYPE);

				//Get Feature Requirement Type URL
				ResourceShape featureInstanceShape = RmUtil.lookupRequirementsInstanceShapes(
						serviceProviderUrl, OSLCConstants.OSLC_RM_V2,
						OSLCConstants.RM_REQUIREMENT_TYPE, client, "Feature");


				ResourceShape collectionInstanceShape = RmUtil.lookupRequirementsInstanceShapes(
						serviceProviderUrl, OSLCConstants.OSLC_RM_V2,
						OSLCConstants.RM_REQUIREMENT_COLLECTION_TYPE, client, "Collection");

				// We need to use Resource shapes to properly handle date attributes attributes,
				// so they aren't interpreted as dateTime.
				// The following 4 lines will enable the logic to properly handle date attributes
				List<ResourceShape> shapes = new ArrayList<ResourceShape>();
				shapes.add(featureInstanceShape);
				shapes.add(collectionInstanceShape);
				OSLC4JUtils.setShapes(shapes);
				OSLC4JUtils.setInferTypeFromShape("true");

				Requirement requirement = null;
				RequirementCollection collection = null;
				URI rootFolder = null;

				String req01URL=null;
				String req02URL=null;
				String req03URL=null;
				String req04URL=null;
				String reqcoll01URL=null;

				String primaryText = null;
				if (( featureInstanceShape != null ) && (requirementFactory != null ) ){

					// Create REQ01
					requirement = new Requirement();
					requirement.setInstanceShape(featureInstanceShape.getAbout());
					requirement.setTitle("Req01");

					// Decorate the PrimaryText
					primaryText = "My Primary Text";
					org.w3c.dom.Element obj = RmUtil.convertStringToHTML(primaryText);
					requirement.getExtendedProperties().put(RmConstants.PROPERTY_PRIMARY_TEXT, obj);

					requirement.setDescription("Created By EclipseLyo");
					requirement.addImplementedBy(new Link(new URI("http://google.com"), "Link in REQ01"));
					//Create the Requirement
					ClientResponse creationResponse = client.createResource(
							requirementFactory, requirement,
							OslcMediaType.APPLICATION_RDF_XML,
							OslcMediaType.APPLICATION_RDF_XML);
					req01URL = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);
					creationResponse.consumeContent();

					// Create REQ02
					requirement = new Requirement();
					requirement.setInstanceShape(featureInstanceShape.getAbout());
					requirement.setTitle("Req02");
					requirement.setDescription("Created By EclipseLyo");
					requirement.addValidatedBy(new Link(new URI("http://bancomer.com"), "Link in REQ02"));
					//Create the change request
					creationResponse = client.createResource(
							requirementFactory, requirement,
							OslcMediaType.APPLICATION_RDF_XML,
							OslcMediaType.APPLICATION_RDF_XML);

					req02URL = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);
					creationResponse.consumeContent();

					// Create REQ03
					requirement = new Requirement();
					requirement.setInstanceShape(featureInstanceShape.getAbout());
					requirement.setTitle("Req03");
					requirement.setDescription("Created By EclipseLyo");
					requirement.addValidatedBy(new Link(new URI("http://outlook.com"), "Link in REQ03"));
					//Create the change request
					creationResponse = client.createResource(
							requirementFactory, requirement,
							OslcMediaType.APPLICATION_RDF_XML,
							OslcMediaType.APPLICATION_RDF_XML);
					req03URL = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);
					creationResponse.consumeContent();

					// Create REQ04
					requirement = new Requirement();
					requirement.setInstanceShape(featureInstanceShape.getAbout());
					requirement.setTitle("Req04");
					requirement.setDescription("Created By EclipseLyo");

					//Create the Requirement
					creationResponse = client.createResource(
							requirementFactory, requirement,
							OslcMediaType.APPLICATION_RDF_XML,
							OslcMediaType.APPLICATION_RDF_XML);
					req04URL = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);
					creationResponse.consumeContent();

					// Now create a collection
					// Create REQ04
					collection = new RequirementCollection();

					collection.addUses(new URI(req03URL));
					collection.addUses(new URI(req04URL));

					collection.setInstanceShape(collectionInstanceShape.getAbout());
					collection.setTitle("Collection01");
					collection.setDescription("Created By EclipseLyo");
					//Create the collection
					creationResponse = client.createResource(
							requirementFactory, collection,
							OslcMediaType.APPLICATION_RDF_XML,
							OslcMediaType.APPLICATION_RDF_XML);
					reqcoll01URL = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);
					creationResponse.consumeContent();

				}

				// Check that everything was properly created
				 if ( req01URL == null ||
					  req02URL == null ||
					  req03URL == null ||
					  req04URL == null ||
					 reqcoll01URL == null ) {
					 throw new Exception("Failed to create an artifact");
				 }

				// GET the root folder based on First requirement created
				ClientResponse getResponse = client.getResource(req01URL,OslcMediaType.APPLICATION_RDF_XML);
				requirement = getResponse.getEntity(Requirement.class);

				// Display attributes based on the Resource shape
				Map<QName, Object> requestExtProperties = requirement.getExtendedProperties();
				for ( QName qname : requestExtProperties.keySet() ){
					Property attr = featureInstanceShape.getProperty(new URI (qname.getNamespaceURI() + qname.getLocalPart()));
					String name = null;
					if ( attr != null){
						name = attr.getTitle();
						if ( name != null ) {
							System.out.println(name  + " = " + requirement.getExtendedProperties().get(qname));
						}
					}
				}


				//Save the URI of the root folder in order to used it easily
				rootFolder = (URI) requirement.getExtendedProperties().get(RmConstants.PROPERTY_PARENT_FOLDER);
				Object changedPrimaryText =  (Object ) requirement.getExtendedProperties().get(RmConstants.PROPERTY_PRIMARY_TEXT);
				if ( changedPrimaryText == null ){
					// Check with the workaround
					 changedPrimaryText =  (Object ) requirement.getExtendedProperties().get(PROPERTY_PRIMARY_TEXT_WORKAROUND);
				}
				String primarytextString = null;
				if (changedPrimaryText != null ) {
					primarytextString = changedPrimaryText.toString(); // Handle the case where Primary Text is returned as XMLLiteral
				}

				if ( ( primarytextString != null) && (! primarytextString.contains(primaryText)) ) {
					logger.log(Level.SEVERE, "Error getting primary Text");
				}

				//QUERIES
				// SCENARIO 01  Do a query for type= Requirement
				OslcQueryParameters queryParams = new OslcQueryParameters();
				queryParams.setPrefix("rdf=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
				queryParams.setWhere("rdf:type=<http://open-services.net/ns/rm#Requirement>");
				OslcQuery query = new OslcQuery(client, queryCapability, 10, queryParams);
				OslcQueryResult result = query.submit();
				boolean processAsJavaObjects = false;
				int resultsSize = result.getMembersUrls().length;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 01 = " + resultsSize + "\n");


				// SCENARIO 02 	Do a query for type= Requirements and for it folder container = rootFolder
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("nav=<http://com.ibm.rdm/navigation#>,rdf=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
				queryParams.setWhere("rdf:type=<http://open-services.net/ns/rm#Requirement> and nav:parent=<" + rootFolder + ">");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				processAsJavaObjects = false;
				resultsSize = result.getMembersUrls().length;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 02 = " + resultsSize + "\n");

				// SCENARIO 03	Do a query for title
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("dcterms=<http://purl.org/dc/terms/>");
				queryParams.setWhere("dcterms:title=\"Req04\"");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				resultsSize = result.getMembersUrls().length;
				processAsJavaObjects = false;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 03 = " + resultsSize + "\n");

				// SCENARIO 04	Do a query for the link that is implemented
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("oslc_rm=<http://open-services.net/ns/rm#>");
				queryParams.setWhere("oslc_rm:implementedBy=<http://google.com>");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				resultsSize = result.getMembersUrls().length;
				processAsJavaObjects = false;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 04 = " + resultsSize + "\n");

				// SCENARIO 05	Do a query for the links that is validated
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("oslc_rm=<http://open-services.net/ns/rm#>");
				queryParams.setWhere("oslc_rm:validatedBy in [<http://bancomer.com>,<http://outlook.com>]");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				resultsSize = result.getMembersUrls().length;
				processAsJavaObjects = false;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 05 = " + resultsSize + "\n");

				// SCENARIO 06 Do a query for it container folder and for the link that is implemented
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("nav=<http://com.ibm.rdm/navigation#>,oslc_rm=<http://open-services.net/ns/rm#>");
				queryParams.setWhere("nav:parent=<"+rootFolder+"> and oslc_rm:validatedBy=<http://bancomer.com>");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				resultsSize = result.getMembersUrls().length;
				processAsJavaObjects = false;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 06 = " + resultsSize + "\n");


				// GET resources from req03 in order edit its values
				getResponse = client.getResource(req03URL,OslcMediaType.APPLICATION_RDF_XML);
				requirement = getResponse.getEntity(Requirement.class);
				// Get the eTAG, we need it to update
				String etag = getResponse.getHeaders().getFirst(OSLCConstants.ETAG);
				getResponse.consumeContent();
				requirement.setTitle("My new Title");
				requirement.addImplementedBy(new Link(new URI("http://google.com"), "Link created by an Eclipse Lyo user"));

				// Update the requirement with the proper etag
				ClientResponse updateResponse = client.updateResource(req03URL,
						requirement, OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_RDF_XML, etag);

				updateResponse.consumeContent();

				/*Do a query in order to see if the requirement have changed*/
				// SCENARIO 07 Do a query for the new title just changed
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("dcterms=<http://purl.org/dc/terms/>");
				queryParams.setWhere("dcterms:title=\"My new Title\"");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				resultsSize = result.getMembersUrls().length;
				processAsJavaObjects = false;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 07 = " + resultsSize + "\n");

				// SCENARIO 08	Do a query for implementedBy links
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("oslc_rm=<http://open-services.net/ns/rm#>");
				queryParams.setWhere("oslc_rm:implementedBy=<http://google.com>");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				resultsSize = result.getMembersUrls().length;
				processAsJavaObjects = false;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for SCENARIO 08 = " + resultsSize + "\n");


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
						   //Requirement req = response.getEntity(Requirement.class);
						   //printRequirementInfo(req);   //print a few attributes
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
