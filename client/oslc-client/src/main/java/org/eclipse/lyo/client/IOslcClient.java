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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client;

import java.util.Map;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.Response;

/**
 * An OSLC Client that extends the JAX-RS 2.0 REST client with OSLC specific CRUD and
 * discovery capabilities. Client applications would typically provide a ClientBuilder
 * to the constructor to configure the REST client too meet their needs.
 */
public interface IOslcClient {

	/**
	 * Returns the JAX-RS client for this OslcClient. Do not touch unless needed.
	 * @return the JAX-RS client
	 */
	public Client getClient();

	/**
	 * Gets an OSLC resource using <code>application/rdf+xml</code>. Use
	 * {@link #getResource(String, String)} to specify the media type or
	 * {@link #getResource(String, Map)} to add other request headers.
	 */
	public Response getResource(String url);

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
	public Response getResource(String url, final String mediaType);

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
	public Response getResource(String url, Map<String, String> requestHeaders);

	public Response getResource (String url, Map<String, String> requestHeaders, String mediaType);

    public Response getResource (String url, Map<String, String> requestHeaders, String mediaType, String configurationContext);

   public Response getResource (String url, Map<String, String> requestHeaders, String mediaType, boolean handleRedirects);

   public Response getResource (String url, Map<String, String> requestHeaders, String mediaType, String configurationContext, boolean handleRedirects);

	/**
	 * Delete an OSLC resource and return a Wink ClientResponse
	 * @param url
	 */
	public Response deleteResource(String url);

	public Response deleteResource(String url, String configurationContext);

	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 */
	public Response createResource(String url, final Object artifact, String mediaType);

	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 */
	public Response createResource(String url, final Object artifact, String mediaType, String acceptType);

	public Response createResource(String url, final Object artifact, String mediaType, String acceptType, String configurationContext);

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	public Response updateResource(String url, final Object artifact, String mediaType);

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType);

	/**
	 * Update (PUT) an artifact to a URL - usually the URL for an existing OSLC artifact
	 */
	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType, String ifMatch);

	public Response updateResource(String url, final Object artifact, String mediaType, String acceptType, String ifMatch, String configurationContext);

}
