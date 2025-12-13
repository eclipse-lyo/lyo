/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation See the NOTICE file(s) distributed with this work for
 * additional information regarding copyright ownership. This program and the accompanying materials are made available
 * under the terms of the Eclipse Public License 2.0 which is available at http://www.eclipse.org/legal/epl-2.0, or the
 * Eclipse Distribution License 1.0 which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.ws.rs.HttpMethod;
import jakarta.ws.rs.client.Client;
import jakarta.ws.rs.client.ClientBuilder;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.client.Invocation.Builder;
import jakarta.ws.rs.client.WebTarget;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;
import jakarta.ws.rs.ext.MessageBodyReader;
import jakarta.ws.rs.ext.MessageBodyWriter;

/**
 * An OSLC Client that extends the JAX-RS 2.0 REST client with OSLC specific CRUD and discovery capabilities. Client
 * applications would typically provide a ClientBuilder to the constructor to configure the REST client too meet their
 * needs.
 */
public class OslcClient implements IOslcClient {

    private final String version;
    private Client client;
    private final static Logger LOGGER = LoggerFactory.getLogger(OslcClient.class);
    private static final String ACCEPT_TYPE_ALL = "*/*";

    /**
     * A simple OslcClient that provides http access to unprotected resources
     */
    public OslcClient() {
        this(ClientBuilder.newBuilder());
    }

    /**
     * @param version OSLC version, see {@link OSLCConstants}
     */
    public OslcClient(String version) {
        this(ClientBuilder.newBuilder(), version);
    }

    /**
     * An OslcClient that allows client applications to provide a configured (but not built) ClientBuilder typically
     * used for supporting https, and various kinds of authentication.
     *
     * @param clientBuilder HTTP client configuration
     */
    public OslcClient(ClientBuilder clientBuilder) {
        this(clientBuilder, OSLCConstants.OSLC2_0);
    }

    /**
     * An OslcClient that allows client applications to provide a configured (but not built) ClientBuilder typically
     * used for supporting https, and various kinds of authentication.
     *
     * @param clientBuilder HTTP client configuration
     * @param version       OSLC version, see {@link OSLCConstants}
     */
    public OslcClient(final ClientBuilder clientBuilder, final String version) {
        this(clientBuilder, version, JenaProvidersRegistry.getProviders());
    }

    /**
     * An OslcClient that allows client applications to provide a configured (but not built) ClientBuilder typically
     * used for supporting https, and various kinds of authentication.
     *
     * @param clientBuilder HTTP client configuration
     * @param version       OSLC version, see {@link OSLCConstants}
     * @param providers     Set of Providers for {@link MessageBodyReader} and {@link MessageBodyWriter}
     */
    public OslcClient(final ClientBuilder clientBuilder, final String version, final Set<Class<?>> providers) {
        this.version = version;
        for (Class<?> provider : providers) {
            clientBuilder.register(provider);
        }
        this.client = clientBuilder.build();
    }

    /**
     * Returns the JAX-RS client for this OslcClient. Do not touch unless needed.
     * 
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
        return getResources(Arrays.asList(links), clazz);
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
     * Gets an OSLC resource using <code>application/rdf+xml</code>. Use {@link #getResource(String, String)} to specify
     * the media type or {@link #getResource(String, Map)} to add other request headers.
     */
    public Response getResource(String url) {
        return getResource(url, (String)null);
    }

    /**
     * Gets an OSLC resource. Use {@link #getResource(String, Map)} instead to add other request headers.
     *
     * @param url       the resource URL
     * @param mediaType the requested media type to use in the HTTP Accept request header
     */
    public Response getResource(String url, final String mediaType) {
        return getResource(url, null, mediaType);
    }

    /**
     * Gets an OSLC resource.
     *
     * @param url            the resource URL
     * @param requestHeaders the HTTP request headers to use. If the <code>Accept</code> header is not in the map, it
     *                       defaults to <code>application/rdf+xml</code>. If <code>OSLC-Core-Version</code> is not in
     *                       the map, it defaults to <code>2.0</code>.
     */
    public Response getResource(String url, Map<String, String> requestHeaders) {
        return getResource(url, requestHeaders, OSLCConstants.CT_RDF, null);
    }

    public Response getResource(String url, Map<String, String> requestHeaders, String defaultMediaType) {
        return getResource(url, requestHeaders, defaultMediaType, null, true);
    }

    public Response getResource(String url, Map<String, String> requestHeaders, String defaultMediaType,
            String configurationContext) {
        return getResource(url, requestHeaders, defaultMediaType, configurationContext, true);
    }

    public Response getResource(String url, Map<String, String> requestHeaders, String defaultMediaType,
            boolean handleRedirects) {
        return getResource(url, requestHeaders, defaultMediaType, null, handleRedirects);
    }

    public Response getResource(String url, Map<String, String> requestHeaders, String defaultMediaType,
            String configurationContext, boolean handleRedirects) {
        return doRequest(HttpMethod.GET, url, null, configurationContext, null, defaultMediaType, OSLCConstants.CT_RDF,
                requestHeaders);
    }


    /**
     * Delete an OSLC resource and return the Response
     * 
     * @param url
     */
    public Response deleteResource(String url) {
        return deleteResource(url, null);
    }
    
    /**
     * Delete an OSLC resource in the given configuration context and return the Response
     */
    public Response deleteResource(String url, String configurationContext) {
        return doRequest(HttpMethod.DELETE, url, null, configurationContext, null, null, null, null);
    }


    /**
     * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
     */
    public Response createResource(String url, final Object artifact, String mediaType) {
        return createResource(url, artifact, mediaType, ACCEPT_TYPE_ALL);
    }

    /**
     * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
     */
    public Response createResource(String url, final Object artifact, String mediaType, String acceptType) {
        return createResource(url, artifact, mediaType, acceptType, null);
    }

    public Response createResource(String url, final Object artifact, String mediaType, String acceptType,
            String configurationContext) {
        return doRequest(HttpMethod.POST, url, artifact, configurationContext, null, mediaType, acceptType, null);
    }

    /**
     * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
     */
    public Response updateResource(String url, final Object artifact, String mediaType) {
        return updateResource(url, artifact, mediaType, ACCEPT_TYPE_ALL);
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
    public Response updateResource(String url, final Object artifact, String mediaType, String acceptType,
            String ifMatch) {
        return updateResource(url, artifact, mediaType, acceptType, ifMatch, null);
    }


    public Response updateResource(String url, final Object artifact, String mediaType, String acceptType,
            String ifMatch, String configurationContext) {
        return doRequest(HttpMethod.PUT, url, artifact, configurationContext, ifMatch, mediaType, acceptType, null);
    }

    Response doRequest(String method, String url, final Object artifact, String configurationContext,
            String ifMatch, String mediaType, String acceptType, Map<String, String> requestHeaders) {
        Response response = null;
        boolean redirect = false;
        do {
            Builder invocationBuilder = client.target(url).request().accept(acceptType);
            addHeaders(invocationBuilder, requestHeaders, ifMatch, configurationContext);
            if (artifact == null) {
                response = invocationBuilder.method(method);
            } else {
                response = invocationBuilder.method(method, Entity.entity(artifact, mediaType));
            }
            if (Response.Status.fromStatusCode(response.getStatus()).getFamily() == Status.Family.REDIRECTION) {
                String newUrl = response.getStringHeaders().getFirst(HttpHeaders.LOCATION);
                LOGGER.trace("Following redirect from {} to {}", url, newUrl);
                url = newUrl;
                response.readEntity(String.class);
                redirect = true;
            } else {
                redirect = false;
            }
        } while (redirect);
        return response;
    }
    
    Map<String, String> addHeaders(Builder invocationBuilder, Map<String, String> customHeaders, String ifMatch,
            String configurationContext) {
        Map<String, String> requestHeaderMap = new HashMap<>();
        // customHeaders may be read only
        if (customHeaders != null) {
            requestHeaderMap.putAll(customHeaders);
        }
        requestHeaderMap.putIfAbsent(OSLCConstants.OSLC_CORE_VERSION, version);
        if (ifMatch != null) {
            requestHeaderMap.putIfAbsent(HttpHeaders.IF_MATCH, ifMatch);
        }
        if (configurationContext != null) {
            requestHeaderMap.putIfAbsent(OSLCConstants.CONFIGURATION_CONTEXT_HEADER, configurationContext);
        }
        requestHeaderMap.entrySet().forEach(h -> invocationBuilder.header(h.getKey(), h.getValue()));
        return requestHeaderMap;
    }

    /**
     * Create a Wink Resource for the given OslcQuery object
     */
    public WebTarget getWebResource(final String capabilityUri) {
        return client.target(capabilityUri);
    }


    /**
     * Lookup the URL of a specific OSLC Service Provider in an OSLC Catalog using the service provider's title
     */
    public String lookupServiceProviderUrl(final String catalogUrl, final String serviceProviderTitle)
            throws IOException, URISyntaxException, ResourceNotFoundException {
        Response response = getResource(catalogUrl, OSLCConstants.CT_RDF);
        if (response.getStatus() == HttpStatus.SC_OK) {
            ServiceProviderCatalog catalog = response.readEntity(ServiceProviderCatalog.class);
            if (catalog != null) {
                for (ServiceProvider sp : catalog.getServiceProviders()) {
                    if (sp.getTitle() != null && sp.getTitle().equalsIgnoreCase(serviceProviderTitle)) {
                        return sp.getAbout().toString();
                    }

                }
            }
            throw new ResourceNotFoundException(catalogUrl, serviceProviderTitle);
        } else {
            LOGGER.warn("Cannot read {} status: {}", catalogUrl, response.getStatus());
            return null;
        }
    }

    /**
     * Find the OSLC Query Capability URL for a given OSLC resource type. If no resource type is given, returns the
     * default Query Capability, if it exists.
     *
     * @param  oslcResourceType - the resource type of the desired query capability. This may differ from the OSLC
     *                          artifact type.
     * @return                  URL of requested Query Capability or null if not found.
     */
    public String lookupQueryCapability(final String serviceProviderUrl, final String oslcDomain,
            final String oslcResourceType) throws IOException, URISyntaxException, ResourceNotFoundException {
        QueryCapability defaultQueryCapability = null;
        QueryCapability firstQueryCapability = null;

        Response response = getResource(serviceProviderUrl, OSLCConstants.CT_RDF);
        ServiceProvider serviceProvider = response.readEntity(ServiceProvider.class);


        if (serviceProvider != null) {
            for (Service service : serviceProvider.getServices()) {
                URI domain = service.getDomain();
                if (domain != null && domain.toString().equals(oslcDomain)) {
                    QueryCapability[] queryCapabilities = service.getQueryCapabilities();
                    if (queryCapabilities != null && queryCapabilities.length > 0) {
                        firstQueryCapability = queryCapabilities[0];
                        for (QueryCapability queryCapability : service.getQueryCapabilities()) {
                            for (URI resourceType : queryCapability.getResourceTypes()) {

                                // return as soon as domain + resource type are matched
                                if (resourceType.toString() != null
                                        && resourceType.toString().equals(oslcResourceType)) {
                                    return queryCapability.getQueryBase().toString();
                                }
                            }
                            // Check if this is the default capability
                            for (URI usage : queryCapability.getUsages()) {
                                if (usage.toString() != null
                                        && usage.toString().equals(OSLCConstants.USAGE_DEFAULT_URI)) {
                                    defaultQueryCapability = queryCapability;
                                }
                            }
                        }
                    }
                }
            }
        }

        // If we reached this point, there was no resource type match
        if (defaultQueryCapability != null) {
            // return default, if present
            return defaultQueryCapability.getQueryBase().toString();
        } else if (firstQueryCapability != null && firstQueryCapability.getResourceTypes().length == 0) {
            // return the first for the domain, if present
            return firstQueryCapability.getQueryBase().toString();
        }

        throw new ResourceNotFoundException(serviceProviderUrl, "QueryCapability");
    }

    public CreationFactory lookupCreationFactoryResource(final String serviceProviderUrl, final String oslcDomain,
            final String oslcResourceType) throws IOException, URISyntaxException, ResourceNotFoundException {
        return lookupCreationFactoryResource(serviceProviderUrl, oslcDomain, oslcResourceType, null);
    }

    public CreationFactory lookupCreationFactoryResource(final String serviceProviderUrl, final String oslcDomain,
            final String oslcResourceType, final String oslcUsage)
            throws IOException, URISyntaxException, ResourceNotFoundException {
        CreationFactory defaultCreationFactory = null;
        CreationFactory firstCreationFactory = null;

        Response response = getResource(serviceProviderUrl, OSLCConstants.CT_RDF);
        ServiceProvider serviceProvider = response.readEntity(ServiceProvider.class);

        if (serviceProvider != null) {
            for (Service service : serviceProvider.getServices()) {
                URI domain = service.getDomain();
                if (domain != null && domain.toString().equals(oslcDomain)) {
                    CreationFactory[] creationFactories = service.getCreationFactories();
                    if (creationFactories != null && creationFactories.length > 0) {
                        firstCreationFactory = creationFactories[0];
                        for (CreationFactory creationFactory : creationFactories) {
                            for (URI resourceType : creationFactory.getResourceTypes()) {

                                // return as soon as domain + resource type are matched
                                if (resourceType.toString() != null
                                        && resourceType.toString().equals(oslcResourceType)) {
                                    // ...but check oslc:usage if requested
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
                            // Check if this is the default factory
                            for (URI usage : creationFactory.getUsages()) {
                                if (usage.toString() != null
                                        && usage.toString().equals(OSLCConstants.USAGE_DEFAULT_URI)) {
                                    defaultCreationFactory = creationFactory;
                                }
                            }
                        }
                    }
                }
            }
        }

        // If we reached this point, there was no resource type match
        if (defaultCreationFactory != null) {
            // return default, if present
            return defaultCreationFactory;
        } else if (firstCreationFactory != null && firstCreationFactory.getResourceTypes().length == 0) {
            // return the first for the domain, if present
            return firstCreationFactory;
        }

        throw new ResourceNotFoundException(serviceProviderUrl, "CreationFactory");
    }

    /**
     * Find the OSLC Creation Factory URL for a given OSLC resource type. If no resource type is given, returns the
     * default Creation Factory, if it exists.
     *
     * @param  oslcResourceType - the resource type of the desired query capability. This may differ from the OSLC
     *                          artifact type.
     * @return                  URL of requested Creation Factory or null if not found.
     */
    public String lookupCreationFactory(final String serviceProviderUrl, final String oslcDomain,
            final String oslcResourceType) throws IOException, URISyntaxException, ResourceNotFoundException {
        return lookupCreationFactory(serviceProviderUrl, oslcDomain, oslcResourceType, null);
    }

    /**
     * Find the OSLC Creation Factory URL for a given OSLC resource type and OSLC usage. If no resource type is given,
     * returns the default Creation Factory, if it exists.
     *
     * @param  oslcResourceType - the resource type of the desired query capability. This may differ from the OSLC
     *                          artifact type.
     * @return                  URL of requested Creation Factory or null if not found.
     */
    public String lookupCreationFactory(final String serviceProviderUrl, final String oslcDomain,
            final String oslcResourceType, final String oslcUsage)
            throws IOException, URISyntaxException, ResourceNotFoundException {
        return lookupCreationFactoryResource(serviceProviderUrl, oslcDomain, oslcResourceType, oslcUsage).getCreation()
                .toString();
    }
}