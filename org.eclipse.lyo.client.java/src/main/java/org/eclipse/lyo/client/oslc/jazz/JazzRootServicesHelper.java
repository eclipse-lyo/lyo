/*******************************************************************************
 * Copyright (c) 2011, 2012 IBM Corporation.
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
package org.eclipse.lyo.client.oslc.jazz;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Logger;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.eclipse.lyo.client.oslc.OslcOAuthClient;
import org.eclipse.lyo.client.oslc.OSLCConstants;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Selector;
import com.hp.hpl.jena.rdf.model.SimpleSelector;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;

/**
 * Helper class to assist in retrieval of attributes from the IBM Rational
 * Jazz rootservices document
 * 
 * This class is not currently thread safe.
 *
 */
public class JazzRootServicesHelper {
	
	private String baseUrl;
	private String rootServicesUrl;
	private String catalogDomain;
	private String catalogNamespace;
	private String catalogProperty;
	private String catalogUrl;
	private Collection<Object []> catalogs = new ArrayList<Object[]>();
	
	//OAuth URLs
	String requestTokenUrl;
	String authorizationTokenUrl;
	String accessTokenUrl;

	private static final String JFS_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/jfs/1.0/";
	private static final String JD_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/discovery/1.0/";
	
	private static final Logger logger = Logger.getLogger(JazzRootServicesHelper.class.getName());
	
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
		this.rootServicesUrl = this.baseUrl + "/rootservices";
		this.catalogDomain = catalogDomain;
		
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
			
		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_AUTO)) {
			
			this.catalogNamespace = OSLCConstants.OSLC_AUTO;
			this.catalogProperty =  JazzRootServicesConstants.AUTO_ROOTSERVICES_CATALOG_PROP;
		
		}
		else {
			logger.severe("Jazz rootservices only supports CM, RM, QM, and Automation catalogs");
		}
				
		processRootServices();
	}
	
	/**
	 * Get the OSLC Catalog URL
	 * @return
	 */
	public String getCatalogUrl()
	{
		return catalogUrl;
	}
	
	/**
	 * 
	 * @param consumerKey
	 * @param secret
	 * @return
	 */
	public OslcOAuthClient initOAuthClient(String consumerKey, String secret) {
		return new OslcOAuthClient (
								requestTokenUrl,
								authorizationTokenUrl,
								accessTokenUrl,
								consumerKey,
								secret);		
	}
	
	/**
	 * 
	 * @param userid
	 * @param password
	 * @return
	 */
	public JazzFormAuthClient initFormClient(String userid, String password)
	{
		return new JazzFormAuthClient(baseUrl, userid, password);
		
	}
	
	/**
	 * 
	 * @param userid
	 * @param password
	 * @param authUrl - the base URL to use for authentication.  This is normally the 
	 * application base URL for RQM and RTC and is the JTS application URL for fronting
	 * applications like RRC and DM. 
	 * @return
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
			Model rdfModel = ModelFactory.createDefaultModel();
			rdfModel.read(is,rootServicesUrl);

			//get the catalog URL
			this.catalogUrl = getRootServicesProperty(rdfModel, this.catalogNamespace, this.catalogProperty);
						
			//get the OAuth URLs
			this.requestTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_REQUEST_TOKEN_URL);
			this.authorizationTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_USER_AUTH_URL);
			this.accessTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, JazzRootServicesConstants.OAUTH_ACCESS_TOKEN_URL);
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

	

}
