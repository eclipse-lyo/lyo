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
 *     Gabriel Ruelas     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.oauth.OAuthException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.http.Header;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.auth.InvalidCredentialsException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.params.HttpClientParams;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;
import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.client.oslc.OAuthRedirectException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.OslcOAuthClient;
import org.eclipse.lyo.client.oslc.jazz.JazzRootServicesHelper;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.client.oslc.resources.OslcQueryParameters;
import org.eclipse.lyo.client.oslc.resources.OslcQueryResult;
import org.eclipse.lyo.client.oslc.resources.Requirement;
import org.eclipse.lyo.client.oslc.resources.RmConstants;
import org.eclipse.lyo.client.oslc.resources.RmUtil;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.AbstractOslcRdfXmlProvider;
import org.w3c.dom.Element;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Selector;
import org.apache.jena.rdf.model.SimpleSelector;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.vocabulary.RDF;

/**
 * Samples of logging in to Doors Web Access and running OSLC operations
 *
 * <ol>
 *    <li>run an OLSC Requirement query and retrieve OSLC Requirements and de-serialize them as Java objects
 *    <li>TODO:  Add more requirement sample scenarios
 * </ol>
 *
 */
public class DoorsOauthSample {

	private static final Logger logger = Logger.getLogger(DoorsOauthSample.class.getName());

	/**
	 * Login to the DWA server and perform some OSLC actions
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
			logger.severe("Example: java DoorsOauthSample -url https://exmple.com:9443/dwa -user ADMIN -password ADMIN -project \"JKE Banking (Requirements Management)\"");
			return;
		}

		String webContextUrl = cmd.getOptionValue("url");
		String user = cmd.getOptionValue("user");
		String passwd = cmd.getOptionValue("password");
		String projectArea = cmd.getOptionValue("project");

		try {

			//STEP 1: Initialize a Jazz rootservices helper and indicate we're looking for the RequirementManagement catalog
			// The root services for DOORs is found at /public level
			JazzRootServicesHelper helper = new JazzRootServicesHelper(webContextUrl + "/public",OSLCConstants.OSLC_RM_V2);

			//STEP 2: Create a new OSLC OAuth capable client, the parameter of following call should be provided
			// by the system administrator of the DOORs Web Access server
			OslcOAuthClient client = helper.initOAuthClient("lyo", "lyo");

			if ( client != null ) {

				//STEP 3: Try to access the context URL to trigger the OAuth dance and login
				try {
					client.getResource(webContextUrl,OSLCConstants.CT_RDF);
				} catch (OAuthRedirectException oauthE) {
					validateTokens(client,	oauthE.getRedirectURL() + "?oauth_token=" + oauthE.getAccessor().requestToken, user, passwd, webContextUrl + "/j_acegi_security_check" );
					// Try to access again
					ClientResponse response = client.getResource(webContextUrl,OSLCConstants.CT_RDF);
					response.getEntity(InputStream.class).close();
				}

				//STEP 4: Get the URL of the OSLC catalog
				String catalogUrl = helper.getCatalogUrl();

				//STEP 5: Find the OSLC Service Provider for the project area we want to work with
				String serviceProviderUrl = lookupServiceProviderUrl(catalogUrl, "Services for " + projectArea, client);

				//STEP 6: Get the Query Capabilities URL so that we can run some OSLC queries
				String queryCapability = client.lookupQueryCapability(serviceProviderUrl,
																	  OSLCConstants.OSLC_RM_V2,
																	  OSLCConstants.RM_REQUIREMENT_TYPE);

				//STEP 7: Get the Creation Factory URL for default Requirements so that we can create one
				Requirement requirement = new Requirement();
				String requirementFactory = client.lookupCreationFactory(
						serviceProviderUrl, OSLCConstants.OSLC_RM_V2,
						requirement.getRdfTypes()[0].toString());

				//STEP 8 Get the default Requirement Type URL
				ResourceShape reqInstanceShape = RmUtil.lookupRequirementsInstanceShapes(
						serviceProviderUrl, OSLCConstants.OSLC_RM_V2,
						requirement.getRdfTypes()[0].toString(), client, "Resource shape for a requirement in the " + projectArea);

				//SCENARIO A: Run a query for all Requirements modified since 08/02/2010 with OSLC paging of 10 items per
				//page turned on and list the members of the result
				OslcQueryParameters queryParamsN = new OslcQueryParameters("dcterms:modified>=\"2010-08-01T21:51:40.979Z\"xsd:dateTime",null,null, null,"dcterms=<http://purl.org/dc/terms/>");

				OslcQuery queryN = new OslcQuery(client, queryCapability, 10, queryParamsN);

				OslcQueryResult resultN = queryN.submit();

				processPagedQueryResults(resultN,client, false);

				System.out.println("\n------------------------------\n");

				if (( reqInstanceShape != null ) && (requirementFactory != null ) ){
					//STEP 9: Create a Requirement
					requirement.setInstanceShape(reqInstanceShape.getAbout());
					// Add a link
					requirement.addImplementedBy(new Link(new URI("http://google.com"), "Link created by an Eclipse Lyo user"));
					requirement.setDescription("Created By EclipseLyo");

					// Add the PrimaryText
					String primaryText = "My Eclipse Lyo CREATED Primary Text";
					Element obj = RmUtil.convertStringToHTML(primaryText);
					requirement.getExtendedProperties().put(RmConstants.PROPERTY_PRIMARY_TEXT, obj);

					//Create the Requirement
					ClientResponse creationResponse = client.createResource(
							requirementFactory, requirement,
							OslcMediaType.APPLICATION_RDF_XML,
							OslcMediaType.APPLICATION_RDF_XML);
					String req01URL = creationResponse.getHeaders().getFirst(HttpHeaders.LOCATION);

					creationResponse.consumeContent();
				}

				//STEP 10: Query of changed values
				OslcQueryParameters queryParams = new OslcQueryParameters();
				queryParams.setPrefix("oslc_rm=<http://open-services.net/ns/rm#>");
				queryParams.setWhere("oslc_rm:implementedBy=<http://google.com>");
				OslcQuery query = new OslcQuery(client, queryCapability, 10, queryParams);
				OslcQueryResult result = query.submit();
				boolean processAsJavaObjects = false;
				int resultsSize = result.getMembersUrls().length;
				processPagedQueryResults(result,client, processAsJavaObjects);
				System.out.println("\n------------------------------\n");
				System.out.println("Number of Results for query 1 = " + resultsSize + "\n");

				//STEP 11: Now get the artifact with identifier = 1
				queryParams = new OslcQueryParameters();
				queryParams.setPrefix("dcterms=<http://purl.org/dc/terms/>");
				queryParams.setWhere("dcterms:identifier=1");
				query = new OslcQuery(client, queryCapability, 10, queryParams);
				result = query.submit();
				String requirementURL = null;
				// Get the URL of returned requirement
				if ( result != null ) {
					String [] returnedURLS = result.getMembersUrls();
					if (( returnedURLS != null ) && ( returnedURLS.length > 0 )) {
						requirementURL = returnedURLS[0];
					}
				}

				// STEP 12 If requirement found, lets get it an modify
				if ( requirementURL != null ) {
					// Get the requirement
					ClientResponse getResponse = client.getResource(requirementURL,OslcMediaType.APPLICATION_RDF_XML);

					// to handle datatype format exception (some system datetime attributes in doors, empty value in some attributes,...)
					// Those attributes will not be parse and not be add in the requirement class
					System.setProperty(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES, "false");

					requirement = getResponse.getEntity(Requirement.class);

						// Change the Primary text
					String primaryText = "My Eclipse Lyo CHANGED Primary Text";
						// Put in the proper object ( Element for XML Strings )
					Element obj = RmUtil.convertStringToHTML(primaryText);
					requirement.getExtendedProperties().put(RmConstants.PROPERTY_PRIMARY_TEXT, obj);

						// Add a couple of links
					requirement.addImplementedBy(new Link(new URI("http://google.com"), "ImplementedBy example"));
					requirement.addElaboratedBy(new Link(new URI("http://terra.com.mx"), "ElaboratedBy example"));

					// Get the eTAG, we need it to update
					String etag = getResponse.getHeaders().getFirst(OSLCConstants.ETAG);
					getResponse.consumeContent();

					/*
					// Following code is needed to workaround an issue in DWA that exposes the Heading inf as encoded XML
					 * No need with DWA 9.6
					{
						// Get the type for "Object Heading"
						org.eclipse.lyo.oslc4j.core.model.Property[] properties = reqInstanceShape.getProperties();
						String attrDef = null;
						for (org.eclipse.lyo.oslc4j.core.model.Property property : properties){
							if (property.getTitle().equalsIgnoreCase("Object Heading")){
								attrDef = property.getPropertyDefinition().toString();
							}
						}
						if ( attrDef != null ) {
							String url = attrDef.substring(0, attrDef.lastIndexOf("/") + 1);
							String name = attrDef.substring(attrDef.lastIndexOf("/") + 1);
							// QName attr12Name = new QName("https://slot12.gdl.mex.ibm.com:8443/dwa/rm/urn:rational::1-514b30b9627b2a31-M-00000062/types/", "attrDef-12");
							QName attr12Name = new QName(url, name);
							String attr12 = (String ) requirement.getExtendedProperties().get(attr12Name);
							attr12 = removeXMLEscape(attr12);
							Element objattr12 = RmUtil.convertStringToHTML(attr12);
							requirement.getExtendedProperties().put(attr12Name, objattr12);
						}
					}
					*/

					// Update the requirement with the proper etag
					ClientResponse updateResponse = client.updateResource(requirementURL,
							requirement, OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_RDF_XML, etag);
					//ClientResponse updateResponse = client.updateResource(requirementURL,
					//		w.toString(), OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_RDF_XML, etag);

					updateResponse.consumeContent();
				}


			}
		} catch (RootServicesException re) {
			logger.log(Level.SEVERE,"Unable to access the Jazz rootservices document at: " + webContextUrl + "/public/rootservices", re);
		} catch (Exception e) {
			logger.log(Level.SEVERE,e.getMessage(),e);
		}




	}

	private static void processPagedQueryResults(OslcQueryResult result, OslcClient client, boolean asJavaObjects) {
		int page = 1;
		//For now, just show first 5 pages
		do {
			System.out.println("\nPage " + page + ":\n");
			result = processCurrentPage(result,client,asJavaObjects);
			if (result.hasNext() && page < 5) {
				result = result.next();
				page++;
			} else {
				break;
			}
		} while(true);
	}

	private static OslcQueryResult processCurrentPage(OslcQueryResult result, OslcClient client, boolean asJavaObjects) {

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
		return result;

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

	/**
	 * Print out the HTTPResponse headers
	 */
	public static void printResponseHeaders(HttpResponse response) {
		Header[] headers = response.getAllHeaders();
		for (int i = 0; i < headers.length; i++) {
			System.out.println("\t- " + headers[i].getName() + ": " + headers[i].getValue());
		}
	}

	public static Map<String, String> getQueryMap(String query) {
		Map<String, String> map = new HashMap<String, String>();
		String[] params = query.split("&"); //$NON-NLS-1$

		for (String param : params) {
		String name = param.split("=")[0]; //$NON-NLS-1$
		String value = param.split("=")[1]; //$NON-NLS-1$
		map.put(name, value);
		}

		return map;
	}

	private static void validateTokens(OslcOAuthClient client, String redirect, String user, String password, String authURL) throws Exception {

		HttpGet request2 = new HttpGet(redirect);
		HttpClientParams.setRedirecting(request2.getParams(), false);
		HttpResponse response = client.getHttpClient().execute(request2);
		EntityUtils.consume(response.getEntity());

		// Get the location
		Header location = response.getFirstHeader("Location");
		HttpGet request3 = new HttpGet(location.getValue());
		HttpClientParams.setRedirecting(request3.getParams(), false);
		response = client.getHttpClient().execute(request3);
		EntityUtils.consume(response.getEntity());

		//POST to login form
		// The server requires an authentication: Create the login form
		// Following line should be like : "https://server:port/dwa/j_acegi_security_check"
		HttpPost formPost = new HttpPost(authURL);
		List<NameValuePair> nvps = new ArrayList<NameValuePair>();
		nvps.add(new BasicNameValuePair("j_username", user));
		nvps.add(new BasicNameValuePair("j_password", password));
		formPost.setEntity(new UrlEncodedFormEntity(nvps, HTTP.UTF_8));

		HttpResponse formResponse = client.getHttpClient().execute(formPost);
		EntityUtils.consume(formResponse.getEntity());

		location = formResponse.getFirstHeader("Location");
		//Third GET
		HttpGet request4 = new HttpGet(location.getValue());
		HttpClientParams.setRedirecting(request4.getParams(), false);
		response = client.getHttpClient().execute(request4);
		EntityUtils.consume(response.getEntity());

		Map<String,String> oAuthMap = getQueryMap(location.getValue());
		String oauthToken = oAuthMap.get("oauth_token");
		String oauthverifier = oAuthMap.get("oauth_verifier");

		// The server requires an authentication: Create the login form
		HttpPost formPost2 = new HttpPost(authURL);
		formPost2.getParams().setParameter("oauth_token", oauthToken);
		formPost2.getParams().setParameter("oauth_verifier", oauthverifier);
		formPost2.getParams().setParameter("authorize", "true");
		formPost2.addHeader("Content-Type","application/x-www-form-urlencoded;charset=UTF-8");

		formResponse = client.getHttpClient().execute(formPost2);
		EntityUtils.consume(formResponse.getEntity());

		Header header = formResponse.getFirstHeader("Content-Length");
		if ((header!=null) && (!("0".equals(header.getValue())))) {
			// The login failed
			throw new InvalidCredentialsException("Authentication failed");
		} else {
			// The login succeed
			// Step (3): Request again the protected resource
			EntityUtils.consume(formResponse.getEntity());
		}
	}

	/**
	 * Lookup the URL of a specific OSLC Service Provider in an OSLC Catalog using the service provider's title
	 *
	 * @param catalogUrl
	 * @param serviceProviderTitle
	 * @return
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws ResourceNotFoundException
	 */
	public static String lookupServiceProviderUrl(final String catalogUrl, final String serviceProviderTitle, final OslcOAuthClient client)
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{
		String retval = null;
		ClientResponse response = client.getResource(catalogUrl,OSLCConstants.CT_RDF);
		Model rdfModel = ModelFactory.createDefaultModel();

		rdfModel.read(response.getEntity(InputStream.class),catalogUrl);

		// Step 1 Check if it is the service provider we are looking for by comparing the name
		ResIterator listResources = rdfModel.listResourcesWithProperty(RDF.type,rdfModel.createResource("http://open-services.net/ns/core#ServiceProvider"));
		Property titleProp = rdfModel.createProperty(OSLCConstants.DC,"title");
		//check each serviceProvider's title and match it to the one passed in
		while (listResources.hasNext()) {
			Resource resource = listResources.next();
			Statement titlestatement = resource.getProperty(titleProp);
			if (titlestatement == null)
				continue;
			String mytitle = titlestatement.getLiteral().getString();
			System.out.println(mytitle);
			if (( mytitle != null) && (mytitle.equalsIgnoreCase(serviceProviderTitle))) {
				System.out.println("Project Found");
				retval =  catalogUrl;
			}
		}

		// Step 2 Check if there are Service providers properties to recursively look in them
		if ( retval == null) {
			Property spPredicate = rdfModel.createProperty(OSLCConstants.OSLC_V2,"serviceProvider");
			Selector select = new SimpleSelector(null, spPredicate, (RDFNode)null);
			StmtIterator listStatements = rdfModel.listStatements(select);

			//check each serviceProvider's title and match it to the one passed in
			while (listStatements.hasNext()) {
				Statement thisSP = listStatements.nextStatement();
				org.apache.jena.rdf.model.Resource spRes = thisSP.getResource();
				if ( spRes.isResource()) {
					// Recursively look for the project Name
					String newURL = spRes.getURI();
					try {
						return lookupServiceProviderUrl(newURL, serviceProviderTitle, client);
					} catch (ResourceNotFoundException nf){

					}
				}
			}

		}

		// Step 3 Check if there are ServiceProvider catalog and recursively look in them
		if ( retval == null) {
			Property spcPredicate = rdfModel.createProperty(OSLCConstants.OSLC_V2,"serviceProviderCatalog");
			Selector select = new SimpleSelector(null, spcPredicate, (RDFNode)null);
			StmtIterator listStatements = rdfModel.listStatements(select);

			//check each serviceProvider's title and match it to the one passed in
			while (listStatements.hasNext()) {
				Statement thisSP = listStatements.nextStatement();
				org.apache.jena.rdf.model.Resource spRes = thisSP.getResource();
				if ( spRes.isResource()) {
					// Recursively look for the project Name
					String newURL = spRes.getURI();
					try {
						return lookupServiceProviderUrl(newURL, serviceProviderTitle, client);
					} catch (ResourceNotFoundException nf){

					}
				}
			}
		}

		if (retval == null ) {
			throw new ResourceNotFoundException(catalogUrl, serviceProviderTitle);
		}

		return retval;
	}


	/**
	 * Find the OSLC Instance Shape URL for a given OSLC resource type.
	 *
	 * @param serviceProviderUrl
	 * @param oslcDomain
	 * @param oslcResourceType - the resource type of the desired query capability.   This may differ from the OSLC artifact type.
	 * @return URL of requested Creation Factory or null if not found.
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws ResourceNotFoundException
	 */
	public static ResourceShape lookupRequirementsInstanceShapesOLD(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, OslcOAuthClient client, String requiredInstanceShape)
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{

		ClientResponse response = client.getResource(serviceProviderUrl,OSLCConstants.CT_RDF);
		ServiceProvider serviceProvider = response.getEntity(ServiceProvider.class);

		if (serviceProvider != null) {
			for (Service service:serviceProvider.getServices()) {
				URI domain = service.getDomain();
				if (domain != null	&& domain.toString().equals(oslcDomain)) {
					CreationFactory [] creationFactories = service.getCreationFactories();
					if (creationFactories != null && creationFactories.length > 0) {
						for (CreationFactory creationFactory:creationFactories) {
							for (URI resourceType:creationFactory.getResourceTypes()) {

								//return as soon as domain + resource type are matched
								if (resourceType.toString() != null && resourceType.toString().equals(oslcResourceType)) {
									URI[] instanceShapes = creationFactory.getResourceShapes();
									if (instanceShapes != null ){
										for ( URI typeURI : instanceShapes) {
											response = client.getResource(typeURI.toString(),OSLCConstants.CT_RDF);
											ResourceShape resourceShape =  response.getEntity(ResourceShape.class);
											String typeTitle = resourceShape.getTitle();
											if ( ( typeTitle != null) && (typeTitle.equalsIgnoreCase(requiredInstanceShape)) ) {
												return resourceShape;
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}


		throw new ResourceNotFoundException(serviceProviderUrl, "InstanceShapes");
	}


	/**
	 * Remove XML Escape indicators
	 *
	 * @param content
	 * @return String
	 */
	public static String removeXMLEscape(String content) {
		content = content.replaceAll("&lt;", "<"); //$NON-NLS-1$ //$NON-NLS-2$
		content = content.replaceAll("&gt;", ">"); //$NON-NLS-1$ //$NON-NLS-2$
		content = content.replaceAll("&quot;", "\""); //$NON-NLS-1$ //$NON-NLS-2$
		content = content.replaceAll("&#039;", "\'"); //$NON-NLS-1$ //$NON-NLS-2$
		content = content.replaceAll("&amp;", "&"); //$NON-NLS-1$ //$NON-NLS-2$
		content = content.trim();
		return content;
	}

}
