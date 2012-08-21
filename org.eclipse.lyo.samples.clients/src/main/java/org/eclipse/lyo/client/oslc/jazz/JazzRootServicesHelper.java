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

import org.apache.wink.client.ClientResponse;
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

public class JazzRootServicesHelper {
	
	private String baseUrl;
	private String rootServicesUrl;
	private String catalogDomain;
	private String catalogUrl;
	private Collection<Object []> catalogs = new ArrayList<Object[]>();
	
	//OAuth URLs
	String requestTokenUrl;
	String authorizationTokenUrl;
	String accessTokenUrl;
	
	

	
	private static final String JFS_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/jfs/1.0/";
	private static final String JD_NAMESPACE = "http://jazz.net/xmlns/prod/jazz/discovery/1.0/";
	
	
	public JazzRootServicesHelper (String url, String catalogDomain) throws RootServicesException {
		this.baseUrl = url;
		this.rootServicesUrl = this.baseUrl + "/rootservices";
		this.catalogDomain = catalogDomain;
		processRootServices();
	}
	
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
								secret);		
	}
	
	public JazzFormAuthClient initFormClient(String userid, String password)
	{
		return new JazzFormAuthClient(baseUrl, userid, password);
		
	}
	
	private void processRootServices() throws RootServicesException
	{
		try {
			OslcClient rootServicesClient = new OslcClient();
			ClientResponse response = rootServicesClient.getResponse(rootServicesUrl, "application/rdf+xml");
			InputStream is = response.getEntity(InputStream.class);
			Model rdfModel = ModelFactory.createDefaultModel();
			rdfModel.read(is,rootServicesUrl);
			
			//get Jazz discover oslcCatalogs resources
			Property catPredicate = rdfModel.createProperty(JD_NAMESPACE,"oslcCatalogs");
            Selector select = new SimpleSelector(null, catPredicate, (RDFNode)null); 
			StmtIterator listStatements = rdfModel.listStatements(select);
			
			//check each oslcCatalog's domain and match it to the one passed on the constructor
			while (listStatements.hasNext()) {
				Statement thisCat = listStatements.nextStatement();
				Resource catalogRes = thisCat.getResource();
				Property domain = rdfModel.createProperty(OSLCConstants.OSLC_V2, "domain");
				String domainValue = catalogRes.getProperty(domain).getObject().toString();
				
				if (domainValue.equals(this.catalogDomain))
				{
					catalogUrl = catalogRes.getURI();
				}
			}
						
			//get the OAuth properties
			requestTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, "oauthRequestTokenUrl");
			authorizationTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, "oauthUserAuthorizationUrl");
			accessTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, "oauthAccessTokenUrl");
		} catch (Exception e) {
			throw new RootServicesException(e);
		}
		
				
	}
	
	private String getRootServicesProperty(Model rdfModel, String namespace, String predicate) {
		String returnVal = null;
				
		Property prop = rdfModel.createProperty(namespace, predicate);
		Statement stmt = rdfModel.getProperty((Resource) null, prop);
		if (stmt != null && stmt.getObject() != null)
			returnVal = stmt.getObject().toString();
		return returnVal;
	}

	

}
