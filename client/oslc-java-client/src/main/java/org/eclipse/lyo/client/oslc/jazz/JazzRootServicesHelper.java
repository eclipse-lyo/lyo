/*******************************************************************************
 * Copyright (c) 2011, 2014 IBM Corporation.
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
 *     Samuel Padgett      - add getter for RDF model so clients can read other services
 *     Samuel Padgett      - add request consumer key and OAuth approval module URLs
 *     Samuel Padgett      - handle trailing '/' in baseUrl
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.jazz;

import java.io.InputStream;

import javax.ws.rs.core.UriBuilder;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.OslcOAuthClient;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class to assist in retrieval of attributes from the IBM Rational
 * Jazz rootservices document
 *
 * This class is not currently thread safe.
 */
@Deprecated
public class JazzRootServicesHelper {
	private String baseUrl;
	private String rootServicesUrl;
	private String catalogDomain;
	private String catalogNamespace;
	private String catalogProperty;
	private String catalogUrl;
	private Model rdfModel;

	//OAuth URLs
	String authorizationRealm;
	String requestTokenUrl;
	String authorizationTokenUrl;
	String accessTokenUrl;
	String requestConsumerKeyUrl;
	String consumerApprovalUrl;

	public static final String JFS_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/jfs/1.0/";
	public static final String JD_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/discovery/1.0/";

	private final static Logger logger = LoggerFactory.getLogger(JazzRootServicesHelper.class);

	/**
	 * Initialize Jazz rootservices-related URLs such as the catalog location and OAuth URLs
	 *
	 * rootservices is unprotected and access does not require authentication
	 *
	 * @param url - base URL of the Jazz server, no including /rootservices.  Example:  https://example.com:9443/ccm
	 * @param catalogDomain - Namespace of the OSLC domain to find the catalog for.  Example:  OSLCConstants.OSLC_CM
	 * @throws RootServicesException
	 */
	public JazzRootServicesHelper (String url, String catalogDomain) throws RootServicesException {
		this.baseUrl = url;
		this.rootServicesUrl = UriBuilder.fromUri(this.baseUrl).path("rootservices").build().toString();
		logger.debug(String.format("Fetching rootservices document at URL <%s>", this.rootServicesUrl));
		this.catalogDomain = catalogDomain;
		logger.debug(String.format("Using catalog domain <%s>", this.catalogDomain));

		if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_CM) ||
		    this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_CM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_CM;
			this.catalogProperty  = JazzRootServicesConstants.CM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_QM) ||
			       this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_QM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_QM;
			this.catalogProperty =  JazzRootServicesConstants.QM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_RM) ||
			       this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_RM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_RM;
			this.catalogProperty =  JazzRootServicesConstants.RM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_AM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_AM_V2;
			this.catalogProperty =  JazzRootServicesConstants.AM_ROOTSERVICES_CATALOG_PROP;

		}
		else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_AUTO)) {

			this.catalogNamespace = OSLCConstants.OSLC_AUTO;
			this.catalogProperty =  JazzRootServicesConstants.AUTO_ROOTSERVICES_CATALOG_PROP;

		}
		else {
			logger.error("Jazz rootservices only supports CM, RM, QM, and Automation catalogs");
		}

		processRootServices();
	}

	/**
	 * Get the OSLC Catalog URL
	 *
	 * @return the catalog URL
	 */
	public String getCatalogUrl()
	{
		return catalogUrl;
	}

	public OslcOAuthClient initOAuthClient(String consumerKey, String secret) {
		return new OslcOAuthClient (
								requestTokenUrl,
								authorizationTokenUrl,
								accessTokenUrl,
								consumerKey,
								secret,
								authorizationRealm );
	}

	public JazzFormAuthClient initFormClient(String userid, String password)
	{
		return new JazzFormAuthClient(baseUrl, userid, password);

	}

	/**
	 * Creates a form auth client for authenticating with the Jazz server.
	 *
	 * @param userid
	 *            the Jazz user ID
	 * @param password
	 *            the Jazz user password or form-based authentication
	 * @param authUrl
	 *            - the base URL to use for authentication. This is normally the
	 *            application base URL for RQM and RTC and is the JTS
	 *            application URL for fronting applications like RRC and DM.
	 *
	 * @return the form auth client
	 */
	public JazzFormAuthClient initFormClient(String userid, String password, String authUrl)
	{
		return new JazzFormAuthClient(baseUrl, authUrl, userid, password);

	}

	private void processRootServices() throws RootServicesException
	{
		try {
			OslcClient rootServicesClient = new OslcClient();
			ClientResponse response = rootServicesClient.getResource(rootServicesUrl,OSLCConstants.CT_RDF);
			InputStream is = response.getEntity(InputStream.class);
			rdfModel = ModelFactory.createDefaultModel();
			rdfModel.read(is,rootServicesUrl);

			//get the catalog URL
			this.catalogUrl = getRootServicesProperty(rdfModel, this.catalogNamespace, this.catalogProperty);

			//get the OAuth URLs
			this.requestTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_REQUEST_TOKEN_URL);
			this.authorizationTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_USER_AUTH_URL);
			this.accessTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_ACCESS_TOKEN_URL);
			try { // Following field is optional, try to get it, if not found ignore exception because it will use the default
				this.authorizationRealm = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_REALM_NAME);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth authorization realm not found in rootservices <%s>", rootServicesUrl));
			}

			try {
				this.requestConsumerKeyUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_REQUEST_CONSUMER_KEY_URL);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth request consumer key URL not found in rootservices <%s>", rootServicesUrl));
			}

			try {
				this.consumerApprovalUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_APPROVAL_MODULE_URL);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth approval module URL not found in rootservices <%s>", rootServicesUrl));
			}
		} catch (Exception e) {
			throw new RootServicesException(this.baseUrl, e);
		}


	}

	private String getRootServicesProperty(Model rdfModel, String namespace, String predicate) throws ResourceNotFoundException {
		String returnVal = null;

		Property prop = rdfModel.createProperty(namespace, predicate);
		Statement stmt = rdfModel.getProperty((Resource) null, prop);
		if (stmt != null && stmt.getObject() != null)
			returnVal = stmt.getObject().toString();
		if (returnVal == null)
		{
			throw new ResourceNotFoundException(baseUrl, namespace + predicate);
		}
		return returnVal;
	}

	/**
	 * Returns the underlying RDF model for the rootservices document. This
	 * allows clients to read other service URLs not directly supported by this
	 * class.
	 *
	 * @return the RDF model
	 */
	public Model getRdfModel() {
		return rdfModel;
	}

	/**
	 * Gets the URL for requesting an OAuth consumer key.
	 *
	 * @return the request consumer key URL
	 * @see <a href="https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2">RootServicesSpecAddendum2</a>
	 */
	public String getRequestConsumerKeyUrl() {
		return requestConsumerKeyUrl;
	}

	/**
	 * Gets the URL for approving an OAuth consumer
	 *
	 * @return the approval URL
	 * @see <a href="https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2">RootServicesSpecAddendum2</a>
	 */
	public String getConsumerApprovalUrl() {
		return consumerApprovalUrl;
	}
}
