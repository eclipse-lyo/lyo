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
package org.eclipse.lyo.client;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.eclipse.lyo.client.exception.ResourceNotFoundException;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.provider.json4j.Json4JProvidersRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * An OSLC Client that extends the JAX-RS 2.0 REST client with OSLC specific CRUD and
 * discovery capabilities. Client applications would typically provide a ClientBuilder
 * to the constructor to configure the REST client too meet their needs.
 */
public class OslcClient implements IOslcClient {

	private final String version;
	private Client client;

	private final static Logger logger = LoggerFactory.getLogger(OslcClient.class);

	/**
	 * A simple OslcClient that provides http access to unprotected resources
	 */
	public OslcClient()
	{
		this(ClientBuilder.newBuilder());
	}

	/**
	 * @param version OSLC version, see {@link OSLCConstants}
	 */
	public OslcClient(String version) {
		this(ClientBuilder.newBuilder(), version);
	}

	/**
	 * An OslcClient that allows client applications to provide a configured (but not built)
	 * ClientBuilder typically used for supporting https, and various kinds of authentication.
	 *
	 * @param clientBuilder HTTP client configuration
	 */
	public OslcClient(ClientBuilder clientBuilder) {
		this(clientBuilder, OSLCConstants.OSLC2_0);
	}

	/**
	 * An OslcClient that allows client applications to provide a configured (but not built)
	 * ClientBuilder typically used for supporting https, and various kinds of authentication.
	 *
	 * @param clientBuilder HTTP client configuration
	 * @param version OSLC version, see {@link OSLCConstants}
	 */
	public OslcClient(ClientBuilder clientBuilder, String version) {
		for (Class<?> provider : JenaProvidersRegistry.getProviders()) {
			clientBuilder.register(provider);
		}
		for (Class<?> provider : Json4JProvidersRegistry.getProviders()) {
			clientBuilder.register(provider);
		}

		this.client = clientBuilder.build();

		this.version = version;
	}

	/**
	 * Returns the JAX-RS client for this OslcClient. Do not touch unless needed.
	 * @return the JAX-RS client
	 */
	public Client getClient() {
		return client;
	}

	/**
	 * Gets an OSLC resource from a given URI string and unwraps a corresponding entity.
	 */
	public <T> T getResource(final String link, final Class<T> clazz) {
		final Response resource = getResource(link.toString());
		return resource.readEntity(clazz);
	}

	/**
	 * Gets an OSLC resource from a given URI and unwraps a corresponding entity.
	 */
	public <T> T getResource(final URI link, final Class<T> clazz) {
		return getResource(link.toString(), clazz);
	}

	/**
	 * Gets an OSLC resource from a URI in a given Link and unwraps a corresponding entity.
	 */
	public <T> T getResource(final Link link, final Class<T> clazz) {
		final URI value = Objects.requireNonNull(link.getValue());

		final Response resource = getResource(value.toString());
		return resource.readEntity(clazz);
	}

	/**
	 * Gets OSLC resources in parallel from an array of URIs and unwraps their corresponding entities.
	 */
	public <T> List<T> getResources(final URI[] links, final Class<T> clazz) {
		return Arrays.stream(links).parallel().map(uri -> {
			final Response resource = getResource(uri.toString());
			return resource.readEntity(clazz);
		}).collect(Collectors.toList());
	}

	/**
	 * Gets OSLC resources in parallel from a collection of URIs and unwraps their corresponding entities.
	 */
	public <T> List<T> getResources(final Collection<URI> links, final Class<T> clazz) {
		return links.parallelStream().map(uri -> {
			final Response resource = getResource(uri.toString());
			return resource.readEntity(clazz);
		}).collect(Collectors.toList());
	}

	/**
	 * Gets OSLC resources in parallel from a set of Links and unwraps their corresponding entities.
	 * <p>
	 * Method renamed due to type erasure in Java
	 */
	public <T> List<T> getResourcesFromLinks(final Collection<Link> links, final Class<T> clazz) {
		final URI[] uris = links.stream().map(l -> l.getValue()).toArray(URI[]::new);
		return getResources(uris, clazz);
	}

	/**
	 * Gets an OSLC resource using <code>application/rdf+xml</code>. Use
	 * {@link #getResource(String, String)} to specify the media type or
	 * {@link #getResource(String, Map)} to add other request headers.
	 */
	@SuppressWarnings("unused")
	public Response getResource(String url) {
		return getResource(url, null, OSLCConstants.CT_RDF, null, true);
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
	@SuppressWarnings("unused")
	public Response getResource(String url, final String mediaType) {
		return getResource(url, null, mediaType, null, true);
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
	public Response getResource(String url, Map<String, String> requestHeaders) {
		return getResource(url, requestHeaders, OSLCConstants.CT_RDF, null, true);
	}

	public Response getResource (String url, Map<String, String> requestHeaders, String defaultMediaType) {
	    return getResource(url, requestHeaders, defaultMediaType, null, true);
	}

    public Response getResource (String url, Map<String, String> requestHeaders, String defaultMediaType,
            String configurationContext) {
       return getResource(url, requestHeaders, defaultMediaType, configurationContext, true);
   }

   public Response getResource (String url, Map<String, String> requestHeaders, String defaultMediaType, boolean handleRedirects) {
       return getResource(url, requestHeaders, defaultMediaType, null, handleRedirects);
   }

   public Response getResource (String url, Map<String, String> requestHeaders, String defaultMediaType,
									String configurationContext, boolean handleRedirects) {
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
				innvocationBuilder.header(OSLCConstants.OSLC_CORE_VERSION, version);
			}

			if(configurationContext != null) {
				innvocationBuilder.header(OSLCConstants.CONFIGURATION_CONTEXT_HEADER, configurationContext);
			}

			response = innvocationBuilder.get();

			if (handleRedirects && Response.Status.fromStatusCode(response.getStatus()).getFamily() == Status.Family.REDIRECTION) {
				url = response.getStringHeaders().getFirst(HttpHeaders.LOCATION);
				logger.debug("Following redirect to {}", url);
//                response.close();
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

	@SuppressWarnings("unused")
	public Response deleteResource(String url) {
		return deleteResource(url, null);
	}

	public Response deleteResource(String url, String configurationContext) {
		Response response = null;
		boolean redirect = false;

		do {
			Builder invocationBuilder = client.target(url).request()
					.header(OSLCConstants.OSLC_CORE_VERSION, version);

			if(configurationContext != null) {
				invocationBuilder.header(OSLCConstants.CONFIGURATION_CONTEXT_HEADER, configurationContext);
			}

			response = invocationBuilder.delete();

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
	@SuppressWarnings("unused")
	public Response createResource(String url, final Object artifact, String mediaType) {
		return createResource(url, artifact, mediaType, "*/*");
	}

	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 */
	@SuppressWarnings("unused")
	public Response createResource(String url, final Object artifact, String mediaType, String acceptType) {
		return createResource(url, artifact, mediaType, acceptType, null);
	}
	@SuppressWarnings("unused")
	public Response createResource(String url, final Object artifact, String mediaType, String acceptType,
								   String configurationContext) {

		Response response = null;
		boolean redirect = false;

		do {
			Builder invocationBuilder = client.target(url).request()
					.accept(acceptType)
					.header(OSLCConstants.OSLC_CORE_VERSION, version);

			if(configurationContext != null) {
				invocationBuilder.header(OSLCConstants.CONFIGURATION_CONTEXT_HEADER, configurationContext);
			}

			response = invocationBuilder
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
	@SuppressWarnings("unused")
	public Response updateResource(String url, final Object artifact, String mediaType) {
		return updateResource(url, artifact, mediaType, "*/*");
	}

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType) {
		return updateResource(url, artifact, mediaType, acceptType, null, null);
	}

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	@SuppressWarnings("unused")
	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType, String ifMatch) {
		return updateResource(url, artifact, mediaType, acceptType, ifMatch, null);
	}


	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType, String ifMatch,
								   String configurationContext) {

		Response response = null;
		boolean redirect = false;

		do {
			Builder invocationBuilder = client.target(url).request()
					.accept(acceptType)
					.header(OSLCConstants.OSLC_CORE_VERSION, version);

			if(ifMatch != null) {
				invocationBuilder.header(HttpHeaders.IF_MATCH, ifMatch);
			}
			if(configurationContext != null) {
				invocationBuilder.header(OSLCConstants.CONFIGURATION_CONTEXT_HEADER, configurationContext);
			}

			response = invocationBuilder
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
		if (response.getStatus() != HttpStatus.SC_OK) {
			logger.warn("Cannot read {} status: {}", catalogUrl, response.getStatus());
			return retval;
		}
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


}
