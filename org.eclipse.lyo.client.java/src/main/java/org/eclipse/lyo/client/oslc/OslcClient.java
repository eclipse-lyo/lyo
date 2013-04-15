/*******************************************************************************
 * Copyright (c) 2011, 2013 IBM Corporation and others.
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
 *     Michael Fiedler                 - initial API and implementation
 *     Lars Ohlen (Tieto Corporation)  - Resolved Bugzilla 393875,389275
 *     Michael Fiedler	               - follow redirects.
 *******************************************************************************/
package org.eclipse.lyo.client.oslc;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.Set;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import net.oauth.OAuthException;
import net.oauth.client.httpclient4.HttpClientPool;

import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.protocol.HttpContext;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.HttpClient;
import org.apache.http.client.RedirectStrategy;
import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.wink.client.ClientConfig;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.RestClient;
import org.apache.wink.client.httpclient.ApacheHttpClientConfig;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.oslc.resources.OslcQuery;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.provider.json4j.Json4JProvidersRegistry;


/**
 * An OSLC Client.  Provides an Apache HttpClient, an Apache Wink REST ClientConfig and defines
 * a getResource method which returns an Apache Wink ClientResponse.
 * 
 * This class is not currently thread safe.
 *
 */


public class OslcClient {
	
	protected DefaultHttpClient httpClient;
	private HttpClientPool clientPool;
	private ClientConfig clientConfig;
	
	private static final String SECURE_SOCKET_PROTOCOL [] = new String[] {"TLS","SSL","SSL_TLS"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$  //$NON-NLS-4$
	
	
	/**
	 * Initialize a new OslcClient using an Apache Http Components 4 Http client and configuration
	 */
	public OslcClient()
	{
		httpClient = new DefaultHttpClient();
		httpClient.setRedirectStrategy(new RedirectStrategy() {
	        public HttpUriRequest getRedirect(HttpRequest request, HttpResponse response, HttpContext context)  {
	            return null;
	        }

	        public boolean isRedirected(HttpRequest request, HttpResponse response, HttpContext context) {
	            return false;
	        }
	    });
		setupLazySSLSupport();
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
	
	/**
	 * Returns the HTTP client associated with this OSLC Client
	 * @return the HTTP client
	 */
	public HttpClient getHttpClient() {
		return httpClient;
	}

	protected HttpClientPool getClientPool() {
		return clientPool;
	}

	protected ClientConfig getClientConfig() {
		return clientConfig;
	}

	/**
	 * Abstract method get an OSLC resource and return a Wink ClientResponse
	 * @param url
	 * @param method
	 * @param mediaType
	 * @return a Wink ClientResponse
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	public ClientResponse getResource(String url, final String mediaType) 
			throws IOException, OAuthException, URISyntaxException {
		
		ClientResponse response = null;
		RestClient restClient = new RestClient(clientConfig);
		boolean redirect = false;
		do {
			response = restClient.resource(url).accept(mediaType).header(OSLCConstants.OSLC_CORE_VERSION,"2.0").get();
			
			if ((response.getStatusCode() == HttpStatus.SC_MOVED_PERMANENTLY) ||
			    (response.getStatusCode() == HttpStatus.SC_MOVED_TEMPORARILY)) {				
				url = response.getHeaders().getFirst("Location");
				response.consumeContent();
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);
		
		return response;
	}
	
	/**
	 * Delete an OSLC resource and return a Wink ClientResponse
	 * @param url
	 * @return a Wink ClientResponse
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	public ClientResponse deleteResource(String url) 
			throws IOException, OAuthException, URISyntaxException {
		
		ClientResponse response = null;
		RestClient restClient = new RestClient(clientConfig);
		boolean redirect = false;
		
		do {
			response = restClient.resource(url).delete();
			
			if ((response.getStatusCode() == HttpStatus.SC_MOVED_PERMANENTLY) ||
			    (response.getStatusCode() == HttpStatus.SC_MOVED_TEMPORARILY)) {				
				url = response.getHeaders().getFirst("Location");
				response.consumeContent();
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);
		
		return response;

	}
	
	
	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 * @param url
	 * @param artifact
	 * @param mediaType
	 * @return
	 */
	public ClientResponse createResource(String url, final Object artifact, String mediaType) {		
		return createResource(url, artifact, mediaType, "*/*");
	}
	
	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 * @param url
	 * @param artifact
	 * @param mediaType
	 * @param acceptType
	 * @return
	 */
	public ClientResponse createResource(String url, final Object artifact, String mediaType, String acceptType) {
		
		ClientResponse response = null;
		RestClient restClient = new RestClient(clientConfig);
		boolean redirect = false;
		
		do {
			response = restClient.resource(url).contentType(mediaType).accept(acceptType).header(OSLCConstants.OSLC_CORE_VERSION,"2.0").post(artifact);
			
			if ((response.getStatusCode() == HttpStatus.SC_MOVED_PERMANENTLY) ||
			    (response.getStatusCode() == HttpStatus.SC_MOVED_TEMPORARILY)) {				
				url = response.getHeaders().getFirst("Location");
				response.consumeContent();
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);
		
		return response;
	}
	
	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 * @param url
	 * @param artifact
	 * @param mediaType
	 * @return
	 */
	public ClientResponse updateResource(String url, final Object artifact, String mediaType) {	
		
		return updateResource(url, artifact, mediaType, "*/*");
	}
	
	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 * @param url
	 * @param artifact
	 * @param mediaType
	 * @param acceptType
	 * @return
	 */
	public ClientResponse updateResource(String url, final Object artifact, String mediaType, String acceptType) {
		
		ClientResponse response = null;
		RestClient restClient = new RestClient(clientConfig);
		boolean redirect = false;
		
		do {
			response = restClient.resource(url).contentType(mediaType).accept(acceptType).header(OSLCConstants.OSLC_CORE_VERSION,"2.0").put(artifact);
			
			if ((response.getStatusCode() == HttpStatus.SC_MOVED_PERMANENTLY) ||
			    (response.getStatusCode() == HttpStatus.SC_MOVED_TEMPORARILY)) {				
				url = response.getHeaders().getFirst("Location");
				response.consumeContent();
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);
		
		return response;
	}

	/**
	 * Create a Wink Resource for the given OslcQuery object
	 * @param query
	 * @return
	 */
	public org.apache.wink.client.Resource getQueryResource(final OslcQuery query) {
		RestClient restClient = new RestClient(clientConfig);
		org.apache.wink.client.Resource resource = restClient.resource(query.getCapabilityUrl());
		return resource;
	}
	
	protected class OAuthHttpPool implements HttpClientPool {
		public HttpClient getHttpClient(URL url) {
			return httpClient;
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
	public String lookupServiceProviderUrl(final String catalogUrl, final String serviceProviderTitle) 
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{
		String retval = null;
		ClientResponse response = getResource(catalogUrl,OSLCConstants.CT_RDF);
		ServiceProviderCatalog catalog = response.getEntity(ServiceProviderCatalog.class);
		
		if (catalog != null) {
			for (ServiceProvider sp:catalog.getServiceProviders()) {
				if (sp.getTitle() != null && sp.getTitle().equalsIgnoreCase(serviceProviderTitle)) {
					retval = sp.getAbout().toString();
					break;
				}
				
			}
		}

		if (retval == null ) {
			throw new ResourceNotFoundException(catalogUrl, serviceProviderTitle);
		}
		
		return retval;
	}
	
	/**
	 * Find the OSLC Query Capability URL for a given OSLC resource type.  If no resource type is given, returns
	 * the default Query Capability, if it exists.
	 *
	 * @param serviceProviderUrl
	 * @param oslcDomain
	 * @param oslcResourceType - the resource type of the desired query capability.   This may differ from the OSLC artifact type.
	 * @return URL of requested Query Capablility or null if not found.
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 * @throws ResourceNotFoundException 
	 */
	public String lookupQueryCapability(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType) 
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{
		QueryCapability defaultQueryCapability = null;
		QueryCapability firstQueryCapability = null;
		
		ClientResponse response = getResource(serviceProviderUrl,OSLCConstants.CT_RDF);
		ServiceProvider serviceProvider = response.getEntity(ServiceProvider.class);
		
		
		if (serviceProvider != null) {
			for (Service service:serviceProvider.getServices()) {
				URI domain = service.getDomain();				
				if (domain != null  && domain.toString().equals(oslcDomain)) {
					QueryCapability [] queryCapabilities = service.getQueryCapabilities();
					if (queryCapabilities != null && queryCapabilities.length > 0) {
						firstQueryCapability = queryCapabilities[0];
						for (QueryCapability queryCapability:service.getQueryCapabilities()) {
							for (URI resourceType:queryCapability.getResourceTypes()) {
								
								//return as soon as domain + resource type are matched
								if (resourceType.toString() != null && resourceType.toString().equals(oslcResourceType)) {
									return queryCapability.getQueryBase().toString();
								}							
							}
							//Check if this is the default capability
							for (URI usage:queryCapability.getUsages()) {
								if (usage.toString() != null && usage.toString().equals(OSLCConstants.USAGE_DEFAULT_URI)) {
									defaultQueryCapability = queryCapability;
								}
							}
						}
					}
				}
			}
		}
		
		//If we reached this point, there was no resource type match
		if (defaultQueryCapability != null) {
			//return default, if present
			return defaultQueryCapability.getQueryBase().toString();
		} else if (firstQueryCapability != null && firstQueryCapability.getResourceTypes().length ==0) {
			//return the first for the domain, if present
			return firstQueryCapability.getQueryBase().toString();
		} 
		
		throw new ResourceNotFoundException(serviceProviderUrl, "QueryCapability");
	}
	
	/**
	 * Find the OSLC Creation Factory URL for a given OSLC resource type.  If no resource type is given, returns
	 * the default Creation Factory, if it exists.  
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
	public String lookupCreationFactory(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType) 
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{
		CreationFactory defaultCreationFactory = null;
		CreationFactory firstCreationFactory = null;
		
		ClientResponse response = getResource(serviceProviderUrl,OSLCConstants.CT_RDF);
		ServiceProvider serviceProvider = response.getEntity(ServiceProvider.class);
				
		if (serviceProvider != null) {
			for (Service service:serviceProvider.getServices()) {
				URI domain = service.getDomain();				
				if (domain != null  && domain.toString().equals(oslcDomain)) {
					CreationFactory [] creationFactories = service.getCreationFactories();
					if (creationFactories != null && creationFactories.length > 0) {
						firstCreationFactory = creationFactories[0];
						for (CreationFactory creationFactory:creationFactories) {
							for (URI resourceType:creationFactory.getResourceTypes()) {
								
								//return as soon as domain + resource type are matched
								if (resourceType.toString() != null && resourceType.toString().equals(oslcResourceType)) {
									return creationFactory.getCreation().toString();
								}							
							}
							//Check if this is the default factory
							for (URI usage:creationFactory.getUsages()) {
								if (usage.toString() != null && usage.toString().equals(OSLCConstants.USAGE_DEFAULT_URI)) {
									defaultCreationFactory = creationFactory;
								}
							}
						}
					}
				}
			}
		}
		
		//If we reached this point, there was no resource type match
		if (defaultCreationFactory != null) {
			//return default, if present
			return defaultCreationFactory.getCreation().toString();
		} else if (firstCreationFactory != null && firstCreationFactory.getResourceTypes().length ==0) {
			//return the first for the domain, if present
			return firstCreationFactory.getCreation().toString();
		} 
		
		throw new ResourceNotFoundException(serviceProviderUrl, "CreationFactory");
	}
	
	
	
	/**
	 * Looks up and select an installed security context provider
	 *  
	 * @return An installed SSLContext Provider
	 * @throws NoSuchAlgorithmException when no suitable provider is installed
	 */
	private SSLContext findInstalledSecurityContext() throws NoSuchAlgorithmException {
		
		// walks through list of secure socked protocols and picks the first found
		// the list is arranged in level of security order
		for (String aSecuredProtocol : SECURE_SOCKET_PROTOCOL) {
			try {
				return SSLContext.getInstance(aSecuredProtocol);
			} catch (NoSuchAlgorithmException e) {
				continue;
			}
		}
		
		throw new NoSuchAlgorithmException("No suitable secured socket provider is installed"); //$NON-NLS-1$
	} 
	
	private void setupLazySSLSupport()   {
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

		
	
				
		try {
			SSLContext sc = findInstalledSecurityContext();
			sc.init(null, trustAllCerts, new java.security.SecureRandom());
			SSLSocketFactory sf = new SSLSocketFactory(sc,SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);
			Scheme https = new Scheme("https", 443, sf); //$NON-NLS-1$
			schemeRegistry.register(https);
		} catch (NoSuchAlgorithmException e) {
			/* Fail Silently */
		} catch (KeyManagementException e) {
			/* Fail Silently */
		}

		
	}
	
	
}
