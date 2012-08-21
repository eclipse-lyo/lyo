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
package org.eclipse.lyo.client.oslc;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import net.oauth.OAuthException;
import net.oauth.client.httpclient4.HttpClientPool;

import org.apache.http.client.HttpClient;
import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.wink.client.ApacheHttpClientConfig;
import org.apache.wink.client.ClientConfig;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.RestClient;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.provider.json4j.Json4JProvidersRegistry;

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
 * An OSLC Client.  Provides an Apache HttpClient, an Apache Wink REST ClientConfig and defines
 * a getResource method which returns an Apache Wink ClientResponse.
 * 
 * 
 * @author mffiedler
 *
 */


public class OslcClient {
	
	protected HttpClient httpClient;
	private HttpClientPool clientPool;
	private ClientConfig clientConfig;
	
	public OslcClient()
	{
		httpClient = new DefaultHttpClient();
		setupLazySSLSupport(httpClient);
		clientPool = new OAuthHttpPool();
		clientConfig = new ApacheHttpClientConfig(httpClient);
		javax.ws.rs.core.Application app = new javax.ws.rs.core.Application() {
		       public Set<Class<?>> getClasses() {
		           Set<Class<?>> classes = new HashSet<Class<?>>();
		           classes.addAll(JenaProvidersRegistry.getProviders());
		           classes.addAll(Json4JProvidersRegistry.getProviders());
		           return classes;
		       }
		};
		clientConfig = clientConfig.applications(app);
		
	}
	
	public HttpClient getHttpClient() {
		return httpClient;
	}

	public HttpClientPool getClientPool() {
		return clientPool;
	}

	public ClientConfig getClientConfig() {
		return clientConfig;
	}

	/**
	 * Abstract method get an OSLC resource and return a Wink ClientResponse
	 * @param url
	 * @param method
	 * @param mediaType
	 * @return
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	
	public ClientResponse getResponse(String url, String mediaType) throws IOException, OAuthException, URISyntaxException {
		

		RestClient restClient = new RestClient(clientConfig);
		return restClient.resource(url).accept(mediaType).header("Oslc-Core-Version","2.0").get();
	}
	
	public org.apache.wink.client.Resource getRemoteResource(OslcQuery query) {
		RestClient restClient = new RestClient(clientConfig);
		org.apache.wink.client.Resource resource = restClient.resource(query.getCapabilityUrl());
		return resource;
	}
	
	public class OAuthHttpPool implements HttpClientPool {
		public HttpClient getHttpClient(URL url) {
			return httpClient;
		}
		
	}
	
	/**
	 * 
	 * @param catalogUrl
	 * @param serviceProviderTitle
	 * @return
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	public String lookupServiceProviderUrl(String catalogUrl, String serviceProviderTitle) throws IOException, OAuthException, URISyntaxException
	{
		String retval = null;
		ClientResponse response = getResponse(catalogUrl,"application/rdf+xml");
		Model rdfModel = ModelFactory.createDefaultModel();

		rdfModel.read(response.getEntity(InputStream.class),catalogUrl);
		
		Property spPredicate = rdfModel.createProperty(OSLCConstants.OSLC_V2,"serviceProvider");
		Selector select = new SimpleSelector(null, spPredicate, (RDFNode)null); 
		StmtIterator listStatements = rdfModel.listStatements(select);
		
		//check each serviceProvider's title and match it to the one passed in
		while (listStatements.hasNext()) {
			Statement thisSP = listStatements.nextStatement();
			com.hp.hpl.jena.rdf.model.Resource spRes = thisSP.getResource();
			Property titleProp = rdfModel.createProperty(OSLCConstants.DC,"title");
			String spTitle = spRes.getProperty(titleProp).getLiteral().getString();
			
			if (spTitle.equals(serviceProviderTitle))
			{
				retval = spRes.getURI();
			}
		}
		
		return retval;
	}
	
	/**
	 * 
	 * @param serviceProviderUrl
	 * @param oslcDomain
	 * @return
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	public String lookupQueryCapability(String serviceProviderUrl, String oslcDomain) throws IOException, OAuthException, URISyntaxException
	{
		String retval = null;
		ClientResponse response = getResponse(serviceProviderUrl,"application/rdf+xml");
		
		Model rdfModel = ModelFactory.createDefaultModel();
		rdfModel.read(response.getEntity(InputStream.class),serviceProviderUrl);
		
        //First, find all services and find the one matching input oslcDomain
		Property spPredicate = rdfModel.createProperty(OSLCConstants.OSLC_V2,"service");
		Selector select = new SimpleSelector(null, spPredicate, (RDFNode)null); 
		StmtIterator listStatements = rdfModel.listStatements(select);
		
		boolean foundQueryCapability = false;
		
		while (listStatements.hasNext()  && !foundQueryCapability) {
			Statement thisService = listStatements.nextStatement();
			com.hp.hpl.jena.rdf.model.Resource  serviceRes = thisService.getResource();
			Property domainProp = rdfModel.createProperty(OSLCConstants.OSLC_V2,"domain");
			String domainValue = serviceRes.getProperty(domainProp).getObject().toString();
			
			//Second, find all queryCapabilities this service provider has
			if (domainValue.equals(oslcDomain)) {
				Property queryCapPredicate = rdfModel.createProperty(OSLCConstants.OSLC_V2,"queryCapability");
				Selector selectQuery = new SimpleSelector(serviceRes, queryCapPredicate, (RDFNode) null);
				StmtIterator queryCaps = rdfModel.listStatements(selectQuery);
				
				//Third, find the service provider with usage = default and return its queryBase
				while (queryCaps.hasNext() && !foundQueryCapability) {
					Statement thisQueryCap = queryCaps.nextStatement();
					com.hp.hpl.jena.rdf.model.Resource queryCapRes = thisQueryCap.getResource();
					Property usageProp = rdfModel.createProperty(OSLCConstants.OSLC_V2,"usage");
					String usageValue = queryCapRes.getProperty(usageProp).getObject().toString();
					if (usageValue.equals(OSLCConstants.OSLC_V2 + "default")) {
						Property queryBaseProp = rdfModel.createProperty(OSLCConstants.OSLC_V2,"queryBase");
						retval = queryCapRes.getProperty(queryBaseProp).getObject().toString();
						foundQueryCapability = true;
					}
				}
			}
		}
		
		return retval;
	}
	
	public Collection<String> getQueryResponseMembers(String queryCapability, ClientResponse response) 
	{
		ArrayList<String> memberList = new ArrayList<String> ();
		
		Model rdfModel = ModelFactory.createDefaultModel();
		rdfModel.read(response.getEntity(InputStream.class),queryCapability);
		
		Property spPredicate = rdfModel.createProperty(OSLCConstants.RDFS,"member");
		com.hp.hpl.jena.rdf.model.Resource queryCapResource = rdfModel.getResource(queryCapability);
		Selector select = new SimpleSelector(queryCapResource, spPredicate, (RDFNode)null); 
		StmtIterator listStatements = rdfModel.listStatements(select);
		while (listStatements.hasNext()) {
			Statement thisMember = listStatements.nextStatement();
			try {
				String memberUrl = thisMember.getResource().getURI();
				memberList.add(memberUrl);
			} catch (Throwable t) {
				//FIXME
				System.err.println("Member was not a resource");
			}
		}
		
		return memberList;
	}
	
	static public void setupLazySSLSupport(HttpClient httpClient) {
		ClientConnectionManager connManager = httpClient.getConnectionManager();
		SchemeRegistry schemeRegistry = connManager.getSchemeRegistry();
		schemeRegistry.unregister("https");
		/** Create a trust manager that does not validate certificate chains */
		TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
			public void checkClientTrusted(
					java.security.cert.X509Certificate[] certs, String authType) {
				/** Ignore Method Call */
			}

			public void checkServerTrusted(
					java.security.cert.X509Certificate[] certs, String authType) {
				/** Ignore Method Call */
			}

			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
				return null;
			}
		} };

		SSLContext sc = null;
		try {
			sc = SSLContext.getInstance("SSL"); //$NON-NLS-1$
			sc.init(null, trustAllCerts, new java.security.SecureRandom());
		} catch (NoSuchAlgorithmException e) {
			/* Fail Silently */
		} catch (KeyManagementException e) {
			/* Fail Silently */
		}

		SSLSocketFactory sf = new SSLSocketFactory(sc);
		sf.setHostnameVerifier(SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);
		Scheme https = new Scheme("https", sf, 443);

		schemeRegistry.register(https);
	}
	
	
}
