/*******************************************************************************
 * Copyright (c) 2018 IBM Corporation and others.
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
 *     Samuel Padgett 	               - support oslc:usage and discovering full creation factory resources
 *     Samuel Padgett                  - use correct trust managers and hostname verifier when updating secure socket protocol
 *     Samuel Padgett                  - don't re-register JAX-RS applications for every request
 *     Samuel Padgett                  - handle any redirect status code
 *     Jad El-khoury                   - Migrate client from Wink to implementation-independent JAX-RS 2.0
 *******************************************************************************/
package org.eclipse.lyo.client.oslc;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;

import org.apache.http.HttpHeaders;
import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.client.exception.RootServicesException;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.provider.json4j.Json4JProvidersRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An OSLC Client that extends the JAX-RS 2.0 REST client with OSLC specific CRUD and
 * discovery capabilities. Client applications would typically provide a ClientBuilder
 * to the constructor to configure the REST client too meet their needs.
 */
public class OslcClient {

	private Client client;

	protected HttpClient httpClient;
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

	private final static Logger logger = LoggerFactory.getLogger(OslcClient.class);





	/**
	 * A simple OslcClient that provides http access to unprotected resources
	 */
	public OslcClient()
	{
		this(ClientBuilder.newBuilder());
	}

	/**
	 * An OslcClient that allows client applications to provide a configured (but not built)
	 * ClientBuilder typically used for supporting https, and various kinds of authentication.
	 *
	 * @param clientBuilder
	 */
	public OslcClient(ClientBuilder clientBuilder)
	{
		for (Class<?> provider : JenaProvidersRegistry.getProviders()) {
			clientBuilder.register(provider);
		}
		for (Class<?> provider : Json4JProvidersRegistry.getProviders()) {
			clientBuilder.register(provider);
		}

		this.client = clientBuilder.build();

		this.httpClient = HttpClientBuilder.create().build();
	}

	/**
	 * Returns the JAX-RS client for this OslcClient
	 * @return the JAX-RS client
	 */
	public Client getClient() {
		return client;
	}

	/**
	 * Returns the HTTP client associated with this OSLC Client
	 * @return the HTTP client
	 */
	public HttpClient getHttpClient() {
		return httpClient;
	}


	/**
	 * Gets an OSLC resource using <code>application/rdf+xml</code>. Use
	 * {@link #getResource(String, String)} to specify the media type or
	 * {@link #getResource(String, Map)} to add other request headers.
	 */
	public Response getResource(String url) throws IOException,
			URISyntaxException {
		return getResource(url, null, OSLCConstants.CT_RDF);
	}

	/**
	 * Gets an OSLC resource. Use {@link #getResource(String, Map)} instead to
	 * add other request headers.
	 *
	 * @param url
	 *            the resource URL
	 * @param mediaType
	 *            the requested media type to use in the HTTP Accept request
	 *            header
	 */
	public Response getResource(String url, final String mediaType)
			throws IOException, URISyntaxException {
		return getResource(url, null, mediaType);
	}

	/**
	 * Gets an OSLC resource.
	 *
	 * @param url
	 *            the resource URL
	 * @param requestHeaders
	 *            the HTTP request headers to use. If the <code>Accept</code>
	 *            header is not in the map, it defaults to
	 *            <code>application/rdf+xml</code>. If
	 *            <code>OSLC-Core-Version</code> is not in the map, it defaults
	 *            to <code>2.0</code>.
	 */
	public Response getResource(String url, Map<String, String> requestHeaders)
			throws IOException, URISyntaxException {
		return getResource(url, requestHeaders, OSLCConstants.CT_RDF);
	}

	protected Response getResource (String url, Map<String, String> requestHeaders, String defaultMediaType)
			throws IOException, URISyntaxException {
		Response response = null;
		boolean redirect = false;
		do {
			WebTarget webTarget = this.client.target(url);
			Builder innvocationBuilder = webTarget.request();
			boolean acceptSet = false;
			boolean versionSet = false;

			// Add in any request headers.
			if (requestHeaders != null) {
				for (Map.Entry<String, String> entry : requestHeaders
						.entrySet()) {
					// Add the header.
					innvocationBuilder.header(entry.getKey(), entry.getValue());

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
				innvocationBuilder.accept(defaultMediaType);
			}

			if (!versionSet) {
				innvocationBuilder.header(OSLCConstants.OSLC_CORE_VERSION, "2.0");
			}

			response = innvocationBuilder.get();

			if (Response.Status.fromStatusCode(response.getStatus()).getFamily() == Status.Family.REDIRECTION) {
				url = response.getStringHeaders().getFirst(HttpHeaders.LOCATION);
				response.readEntity(String.class);
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
	 */
	public Response deleteResource(String url)
			throws IOException, URISyntaxException {
		Response response = null;
		boolean redirect = false;

		do {
			response = client.target(url).request()
					.header(OSLCConstants.OSLC_CORE_VERSION,"2.0")
					.delete();

			if (Response.Status.fromStatusCode(response.getStatus()).getFamily() == Status.Family.REDIRECTION) {
				url = response.getStringHeaders().getFirst(HttpHeaders.LOCATION);
				response.readEntity(String.class);
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);

		return response;

	}


	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 */
	public Response createResource(String url, final Object artifact, String mediaType) throws IOException, URISyntaxException {
		return createResource(url, artifact, mediaType, "*/*");
	}

	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 */
	public Response createResource(String url, final Object artifact, String mediaType, String acceptType) throws IOException, URISyntaxException {

		Response response = null;
		boolean redirect = false;

		do {
			response = client.target(url).request()
					.accept(acceptType)
					.header(OSLCConstants.OSLC_CORE_VERSION,"2.0")
					.post(Entity.entity(artifact, mediaType));

			if (Response.Status.fromStatusCode(response.getStatus()).getFamily() == Status.Family.REDIRECTION) {
				url = response.getStringHeaders().getFirst(HttpHeaders.LOCATION);
				response.readEntity(String.class);
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);

		return response;
	}

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	public Response updateResource(String url, final Object artifact, String mediaType) {

		return updateResource(url, artifact, mediaType, "*/*");
	}

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType) {

		Response response = null;
		boolean redirect = false;

		do {
			response = client.target(url).request()
					.accept(acceptType)
					.header(OSLCConstants.OSLC_CORE_VERSION,"2.0")
					.put(Entity.entity(artifact, mediaType));

			if (Response.Status.fromStatusCode(response.getStatus()).getFamily() == Status.Family.REDIRECTION) {
				url = response.getStringHeaders().getFirst(HttpHeaders.LOCATION);
				response.readEntity(String.class);
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);

		return response;
	}

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType, String ifMatch) throws IOException, URISyntaxException {

		Response response = null;
		boolean redirect = false;

		do {
			response = client.target(url).request()
					.accept(acceptType)
					.header(OSLCConstants.OSLC_CORE_VERSION,"2.0").header(HttpHeaders.IF_MATCH, ifMatch)
					.put(Entity.entity(artifact, mediaType));

			if (Response.Status.fromStatusCode(response.getStatus()).getFamily() == Status.Family.REDIRECTION) {
				url = response.getStringHeaders().getFirst(HttpHeaders.LOCATION);
				response.readEntity(String.class);
				redirect = true;
			} else {
				redirect = false;
			}
		} while (redirect);

		return response;
	}

	/**
	 * Create a Wink Resource for the given OslcQuery object
	 */
	public WebTarget getWebResource(final String capabilityUri) {
		WebTarget webTarget = client.target(capabilityUri);
		return webTarget;
	}


	/**
	 * Lookup the URL of a specific OSLC Service Provider in an OSLC Catalog using the service provider's title
	 */
	public String lookupServiceProviderUrl(final String catalogUrl, final String serviceProviderTitle)
			throws IOException, URISyntaxException, ResourceNotFoundException
	{
		String retval = null;
		Response response = getResource(catalogUrl,OSLCConstants.CT_RDF);
		ServiceProviderCatalog catalog = response.readEntity(ServiceProviderCatalog.class);

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
	 * @param oslcResourceType - the resource type of the desired query capability.   This may differ from the OSLC artifact type.
	 * @return URL of requested Query Capability or null if not found.
	 */
	public String lookupQueryCapability(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType)
			throws IOException, URISyntaxException, ResourceNotFoundException
	{
		QueryCapability defaultQueryCapability = null;
		QueryCapability firstQueryCapability = null;

		Response response = getResource(serviceProviderUrl,OSLCConstants.CT_RDF);
		ServiceProvider serviceProvider = response.readEntity(ServiceProvider.class);


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
			throws IOException, URISyntaxException, ResourceNotFoundException
	{
		return lookupCreationFactoryResource(serviceProviderUrl, oslcDomain, oslcResourceType, null);
	}

	public CreationFactory lookupCreationFactoryResource(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, final String oslcUsage)
			throws IOException, URISyntaxException, ResourceNotFoundException
	{
		CreationFactory defaultCreationFactory = null;
		CreationFactory firstCreationFactory = null;

		Response response = getResource(serviceProviderUrl,OSLCConstants.CT_RDF);
		ServiceProvider serviceProvider = response.readEntity(ServiceProvider.class);

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
	 * @param oslcResourceType - the resource type of the desired query capability.   This may differ from the OSLC artifact type.
	 * @return URL of requested Creation Factory or null if not found.
	 */
	public String lookupCreationFactory(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType)
			throws IOException, URISyntaxException, ResourceNotFoundException
	{
		return lookupCreationFactory(serviceProviderUrl, oslcDomain, oslcResourceType, null);
	}

	/**
	 * Find the OSLC Creation Factory URL for a given OSLC resource type and OSLC usage.  If no resource type is given, returns
	 * the default Creation Factory, if it exists.
	 *
	 * @param oslcResourceType - the resource type of the desired query capability.   This may differ from the OSLC artifact type.
	 * @return URL of requested Creation Factory or null if not found.
	 */
	public String lookupCreationFactory(final String serviceProviderUrl, final String oslcDomain, final String oslcResourceType, final String oslcUsage)
			throws IOException, URISyntaxException, ResourceNotFoundException
	{
		return lookupCreationFactoryResource(serviceProviderUrl, oslcDomain, oslcResourceType, oslcUsage).getCreation().toString();
	}

	/**
	 * Get the OSLC Catalog URL
	 *
	 * @return the catalog URL
	 */
	public String getCatalogUrl(String webContextUrl, String catalogDomain) throws RootServicesException
	{
		this.baseUrl = webContextUrl;
		this.rootServicesUrl = UriBuilder.fromUri(this.baseUrl).path("rootservices").build().toString();
		logger.debug(String.format("Fetching rootservices document at URL <%s>", this.rootServicesUrl));
		this.catalogDomain = catalogDomain;
		logger.debug(String.format("Using catalog domain <%s>", this.catalogDomain));

		if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_CM) ||
		    this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_CM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_CM;
			this.catalogProperty  = RootServicesConstants.CM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_QM) ||
			       this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_QM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_QM;
			this.catalogProperty =  RootServicesConstants.QM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_RM) ||
			       this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_RM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_RM;
			this.catalogProperty =  RootServicesConstants.RM_ROOTSERVICES_CATALOG_PROP;

		} else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_AM_V2)) {

			this.catalogNamespace = OSLCConstants.OSLC_AM_V2;
			this.catalogProperty =  RootServicesConstants.AM_ROOTSERVICES_CATALOG_PROP;

		}
		else if (this.catalogDomain.equalsIgnoreCase(OSLCConstants.OSLC_AUTO)) {

			this.catalogNamespace = OSLCConstants.OSLC_AUTO;
			this.catalogProperty =  RootServicesConstants.AUTO_ROOTSERVICES_CATALOG_PROP;

		}
		else {
			logger.error("Jazz rootservices only supports CM, RM, QM, and Automation catalogs");
		}

		try {
			Response response = this.getResource(rootServicesUrl,OSLCConstants.CT_RDF);
			InputStream is = response.readEntity(InputStream.class);
			rdfModel = ModelFactory.createDefaultModel();
			rdfModel.read(is,rootServicesUrl);

			//get the catalog URL
			this.catalogUrl = getRootServicesProperty(rdfModel, this.catalogNamespace, this.catalogProperty);

			//get the OAuth URLs
			this.requestTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_REQUEST_TOKEN_URL);
			this.authorizationTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_USER_AUTH_URL);
			this.accessTokenUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_ACCESS_TOKEN_URL);
			try { // Following field is optional, try to get it, if not found ignore exception because it will use the default
				this.authorizationRealm = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_REALM_NAME);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth authorization realm not found in rootservices <%s>", rootServicesUrl));
			}

			try {
				this.requestConsumerKeyUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_REQUEST_CONSUMER_KEY_URL);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth request consumer key URL not found in rootservices <%s>", rootServicesUrl));
			}

			try {
				this.consumerApprovalUrl = getRootServicesProperty(rdfModel, JFS_NAMESPACE, RootServicesConstants.OAUTH_APPROVAL_MODULE_URL);
			} catch (ResourceNotFoundException e) {
				logger.debug(String.format("OAuth approval module URL not found in rootservices <%s>", rootServicesUrl));
			}
		} catch (Exception e) {
			throw new RootServicesException(this.baseUrl, e);
		}

		return this.catalogUrl;
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
