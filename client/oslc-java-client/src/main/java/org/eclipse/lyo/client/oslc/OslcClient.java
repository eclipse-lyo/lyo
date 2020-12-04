/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.client.oslc;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import javax.ws.rs.core.Response.Status;

import net.oauth.OAuthException;
import net.oauth.client.httpclient4.HttpClientPool;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.RedirectStrategy;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.conn.ssl.X509HostnameVerifier;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager;
import org.apache.http.protocol.HttpContext;
import org.apache.wink.client.ClientConfig;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.Resource;
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
 */
@Deprecated
public class OslcClient {

	protected DefaultHttpClient httpClient;
	private HttpClientPool clientPool;
	private ClientConfig clientConfig;
	private String configuredSecureSocketProtocol;
	private TrustManager[] trustManagers;
	X509HostnameVerifier hostnameVerifier;

	/**
	 * Returns theSecure Sockect Protocol associated with this OSLC Client
	 * @return the user configured Secure Socket Protocol
	 */
	public String getConfiguredSecureSocketProtocol() {
		return configuredSecureSocketProtocol;
	}

	/**
	 * Sets the Secure Socket Protocol to be used, valid values "TLSv1.2","TLS","SSL","SSL_TLS".
	 */
	public void setConfiguredSecureSocketProtocol(
			String configuredSecureSocketProtocol) {
		this.configuredSecureSocketProtocol = configuredSecureSocketProtocol;

		// Make sure to update the trust managers and hostname verifier for the new protocol.
		setupSSLSupport();
	}

	private static final String SECURE_SOCKET_PROTOCOL [] = new String[] {"TLSv1.2","TLS","SSL","SSL_TLS"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$  //$NON-NLS-4$

	/**
	 * Initialize a new OslcClient using an Apache Http Components 4 Http client and configuration.
	 */
	public OslcClient()
	{
		this((TrustManager[])null, (X509HostnameVerifier)null);
	}

	/**
	 * Initialize a new OslcClient using an Apache Http Components 4 Http client and configuration.
	 * Use the provided TrustManagers and X509HostnameVerifiers instead of the defaults which do no verification;
	 */
	public OslcClient(TrustManager [] trustManagers, X509HostnameVerifier hostnameVerifier)
	{
		httpClient = new DefaultHttpClient(new ThreadSafeClientConnManager());
		httpClient.setRedirectStrategy(new RedirectStrategy() {
			@Override
			public HttpUriRequest getRedirect(HttpRequest request, HttpResponse response, HttpContext context)  {
				return null;
			}

			@Override
			public boolean isRedirected(HttpRequest request, HttpResponse response, HttpContext context) {
				return false;
			}
		});
		this.trustManagers = trustManagers;
		this.hostnameVerifier = hostnameVerifier;
		setupSSLSupport();
		clientPool = new OAuthHttpPool();
		clientConfig = new ApacheHttpClientConfig(httpClient);
		javax.ws.rs.core.Application app = new javax.ws.rs.core.Application() {
			@Override
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
	 * Gets an OSLC resource using <code>application/rdf+xml</code>. Use
	 * {@link #getResource(String, String)} to specify the media type or
	 * {@link #getResource(String, Map)} to add other request headers. Call
	 * {@link ClientResponse#getEntity(Class)} on the response to get the
	 * OSLC4J-annotated Java object.
	 *
	 * @param url
	 *            the resource URL
	 * @return the Apache Wink ClientResponse
	 *
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	public ClientResponse getResource(String url) throws IOException,
			OAuthException, URISyntaxException {
		return getResource(url, null, OSLCConstants.CT_RDF);
	}

	/**
	 * Gets an OSLC resource. Use {@link #getResource(String, Map)} instead to
	 * add other request headers. Call {@link ClientResponse#getEntity(Class)}
	 * on the response to get the OSLC4J-annotated Java object.
	 *
	 * @param url
	 *            the resource URL
	 * @param mediaType
	 *            the requested media type to use in the HTTP Accept request
	 *            header
	 * @return the Apache Wink ClientResponse
	 *
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	public ClientResponse getResource(String url, final String mediaType)
			throws IOException, OAuthException, URISyntaxException {
		return getResource(url, null, mediaType);
	}

	/**
	 * Gets an OSLC resource. Call {@link ClientResponse#getEntity(Class)} on
	 * the response to get the OSLC4J-annotated Java object.
	 *
	 * @param url
	 *            the resource URL
	 * @param requestHeaders
	 *            the HTTP request headers to use. If the <code>Accept</code>
	 *            header is not in the map, it defaults to
	 *            <code>application/rdf+xml</code>. If
	 *            <code>OSLC-Core-Version</code> is not in the map, it defaults
	 *            to <code>2.0</code>.
	 * @return the Apache Wink ClientResponse
	 *
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	public ClientResponse getResource(String url, Map<String, String> requestHeaders)
			throws IOException, OAuthException, URISyntaxException {
		return getResource(url, requestHeaders, OSLCConstants.CT_RDF);
	}

	protected ClientResponse getResource(String url, Map<String, String> requestHeaders, String defaultMediaType)
			throws IOException, OAuthException, URISyntaxException {
		ClientResponse response = null;
		RestClient restClient = new RestClient(clientConfig);
		boolean redirect = false;
		do {
			Resource resource = restClient.resource(url);
			boolean acceptSet = false;
			boolean versionSet = false;

			// Add in any request headers.
			if (requestHeaders != null) {
				for (Map.Entry<String, String> entry : requestHeaders
						.entrySet()) {
					// Add the header.
					resource.header(entry.getKey(), entry.getValue());

					// Remember if we've already set Accept or
					// OSLC-Core-Version.
					if ("accept".equalsIgnoreCase(entry.getKey())) {
						acceptSet = true;
					}

					if (OSLCConstants.OSLC_CORE_VERSION.equalsIgnoreCase(entry
							.getKey())) {
						versionSet = true;
					}
				}
			}

			// Make sure both the Accept and OSLC-Core-Version headers have been
			// set.
			if (!acceptSet) {
				resource.accept(defaultMediaType);
			}

			if (!versionSet) {
				resource.header(OSLCConstants.OSLC_CORE_VERSION, "2.0");
			}

			response = resource.get();

			if (response.getStatusType().getFamily() == Status.Family.REDIRECTION) {
				url = response.getHeaders().getFirst(HttpHeaders.LOCATION);
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
			response = restClient.resource(url).header(OSLCConstants.OSLC_CORE_VERSION,"2.0").delete();

			if (response.getStatusType().getFamily() == Status.Family.REDIRECTION) {
				url = response.getHeaders().getFirst(HttpHeaders.LOCATION);
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
	 * @throws URISyntaxException
	 * @throws OAuthException
	 * @throws IOException
	 */
	public ClientResponse createResource(String url, final Object artifact, String mediaType) throws IOException, OAuthException, URISyntaxException {
		return createResource(url, artifact, mediaType, "*/*");
	}

	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 * @param url
	 * @param artifact
	 * @param mediaType
	 * @param acceptType
	 * @return
	 * @throws URISyntaxException
	 * @throws OAuthException
	 * @throws IOException
	 */
	public ClientResponse createResource(String url, final Object artifact, String mediaType, String acceptType) throws IOException, OAuthException, URISyntaxException {

		ClientResponse response = null;
		RestClient restClient = new RestClient(clientConfig);
		boolean redirect = false;

		do {
			response = restClient.resource(url).contentType(mediaType).accept(acceptType).header(OSLCConstants.OSLC_CORE_VERSION,"2.0").post(artifact);

			if (response.getStatusType().getFamily() == Status.Family.REDIRECTION) {
				url = response.getHeaders().getFirst(HttpHeaders.LOCATION);
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

			if (response.getStatusType().getFamily() == Status.Family.REDIRECTION) {
				url = response.getHeaders().getFirst(HttpHeaders.LOCATION);
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
	 * @param acceptType
	 * @return
	 * @throws URISyntaxException
	 * @throws OAuthException
	 * @throws IOException
	 */
	public ClientResponse updateResource(String url, final Object artifact, String mediaType, String acceptType, String ifMatch) throws IOException, OAuthException, URISyntaxException {

		ClientResponse response = null;
		RestClient restClient = new RestClient(clientConfig);
		boolean redirect = false;

		do {
			response = restClient.resource(url).contentType(mediaType).accept(acceptType)
					.header(OSLCConstants.OSLC_CORE_VERSION,"2.0").header(HttpHeaders.IF_MATCH, ifMatch).put(artifact);

			if (response.getStatusType().getFamily() == Status.Family.REDIRECTION) {
				url = response.getHeaders().getFirst(HttpHeaders.LOCATION);
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
		@Override
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

	public CreationFactory lookupCreationFactoryResource(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType)
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{
		return lookupCreationFactoryResource(serviceProviderUrl, oslcDomain, oslcResourceType, null);
	}

	public CreationFactory lookupCreationFactoryResource(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, final String oslcUsage)
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
									//...but check oslc:usage if requested
									if (oslcUsage != null) {
										for (URI factoryUsage : creationFactory.getUsages()) {
											if (oslcUsage.equals(factoryUsage.toString())) {
												return creationFactory;
											}
										}
									} else {
										return creationFactory;
									}
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
			return defaultCreationFactory;
		} else if (firstCreationFactory != null && firstCreationFactory.getResourceTypes().length ==0) {
			//return the first for the domain, if present
			return firstCreationFactory;
		}

		throw new ResourceNotFoundException(serviceProviderUrl, "CreationFactory");
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
		return lookupCreationFactory(serviceProviderUrl, oslcDomain, oslcResourceType, null);
	}

	/**
	 * Find the OSLC Creation Factory URL for a given OSLC resource type and OSLC usage.  If no resource type is given, returns
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
	public String lookupCreationFactory(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, final String oslcUsage)
			throws IOException, OAuthException, URISyntaxException, ResourceNotFoundException
	{
		return lookupCreationFactoryResource(serviceProviderUrl, oslcDomain, oslcResourceType, oslcUsage).getCreation().toString();
	}

	/**
	 * Looks up and select an installed security context provider
	 *
	 * @return An installed SSLContext Provider
	 * @throws NoSuchAlgorithmException when no suitable provider is installed
	 */
	private SSLContext findInstalledSecurityContext() throws NoSuchAlgorithmException {

		if ( configuredSecureSocketProtocol != null ) {
			SSLContext sslContext = null;
			try {
				sslContext = SSLContext.getInstance(configuredSecureSocketProtocol);
			}
			catch (NoSuchAlgorithmException e) {
				// Ignore Exception, we will try other default values below
			}
			if ( sslContext != null ){
				return sslContext;
			}
		}

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

	private void setupSSLSupport()   {
		ClientConnectionManager connManager = httpClient.getConnectionManager();
		SchemeRegistry schemeRegistry = connManager.getSchemeRegistry();
		schemeRegistry.unregister("https");
		/** Create a trust manager that does not validate certificate chains */
		TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
			@Override
			public void checkClientTrusted(
					java.security.cert.X509Certificate[] certs, String authType) {
				/** Ignore Method Call */
			}

			@Override
			public void checkServerTrusted(
					java.security.cert.X509Certificate[] certs, String authType) {
				/** Ignore Method Call */
			}

			@Override
			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
				return null;
			}
		} };

		try {
			SSLContext sc = findInstalledSecurityContext();
			if (trustManagers == null) {
				trustManagers = trustAllCerts;
			}
			if (hostnameVerifier == null) {
				hostnameVerifier = SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER;
			}
			sc.init(null, trustManagers, new java.security.SecureRandom());
			SSLSocketFactory sf = new SSLSocketFactory(sc,hostnameVerifier);
			Scheme https = new Scheme("https", 443, sf); //$NON-NLS-1$
			schemeRegistry.register(https);
		} catch (NoSuchAlgorithmException e) {
			/* Fail Silently */
		} catch (KeyManagementException e) {
			/* Fail Silently */
		}


	}


}
