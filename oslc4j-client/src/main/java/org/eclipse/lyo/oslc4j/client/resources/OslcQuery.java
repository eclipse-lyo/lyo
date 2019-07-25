/*
 * Copyright (c) 2012 IBM Corporation and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.oslc4j.client.resources;


import javax.ws.rs.core.Response;

import org.eclipse.lyo.oslc4j.client.OSLCConstants;
import org.eclipse.lyo.oslc4j.client.OslcClient;

import javax.ws.rs.client.WebTarget;

/**
 * Represents an OSLC query (HTTP GET) request to be made of a remote system.
 *
 * Immutable.
 */

@SuppressWarnings("WeakerAccess")
public class OslcQuery {

	private final OslcClient oslcClient;

	private final String capabilityUrl;

	private String queryUrl;

	private final int pageSize;

	private WebTarget queryResource;

	//query parameters
	private final String where;
	private final String select;
	private final String orderBy;
	private final String searchTerms;
	private final String prefix;
	private final String version;

	/**
	 * Create an OSLC query that uses the remote system's default page size.
	 *
	 * @param oslcClient the authenticated OSLC client
	 * @param capabilityUrl the URL that is the base
	 */
	public OslcQuery(OslcClient oslcClient, String capabilityUrl) {
		this(oslcClient, capabilityUrl, 0);
	}

	/**
	 * Create an OSLC query with query parameters that uses the default page size
	 * @param oslcClient the authenticated OSLC client
	 * @param capabilityUrl capabilityUrl the URL that is the base
	 * @param oslcQueryParams an OslcQueryParameters object
	 * @see OslcQueryParameters
	 */
	public OslcQuery(OslcClient oslcClient, String capabilityUrl, OslcQueryParameters oslcQueryParams) {
		this(oslcClient, capabilityUrl, 0, oslcQueryParams);
	}

	/**
	 * Create an OSLC query that uses the given page size
	 *
	 * @param oslcClient the authenticated OSLC client
	 * @param capabilityUrl the URL that is the base
	 * @param pageSize the number of results to include on each page (OslcQueryResult)
	 *
	 */
	public OslcQuery(OslcClient oslcClient, String capabilityUrl, int pageSize) {
		this(oslcClient, capabilityUrl, pageSize, null);
	}

	/**
	 * Create an OSLC query that uses OSLC query parameters and the given page size
	 *
	 * @param oslcClient the authenticated OSLC client
	 * @param capabilityUrl the URL that is the base
	 * @param pageSize the number of results to include on each page (OslcQueryResult)
	 * @param oslcQueryParams an OslcQueryParameters object (see {@link OslcQueryParameters})
	 */
	public OslcQuery(OslcClient oslcClient, String capabilityUrl,
			int pageSize, OslcQueryParameters oslcQueryParams) {
		this(oslcClient, capabilityUrl, pageSize, oslcQueryParams, OSLCConstants.OSLC2_0);
	}

	/**
	 * Create an OSLC query that uses OSLC query parameters and the given page size
	 *
	 * @param oslcClient the authenticated OSLC client
	 * @param capabilityUrl the URL that is the base
	 * @param pageSize the number of results to include on each page (OslcQueryResult)
	 * @param oslcQueryParams an OslcQueryParameters object (see {@link OslcQueryParameters})
	 * @param version OSLC Version (see {@link OSLCConstants})
	 */
	public OslcQuery(OslcClient oslcClient, String capabilityUrl,
					 int pageSize, OslcQueryParameters oslcQueryParams, String version) {
		this.oslcClient = oslcClient;
		this.capabilityUrl = capabilityUrl;
		this.pageSize = (pageSize < 1) ? 0 : pageSize;

		//make a local copy of any query parameters
		if (oslcQueryParams != null)
		{
			this.where = oslcQueryParams.getWhere();
			this.select = oslcQueryParams.getSelect();
			this.orderBy = oslcQueryParams.getOrderBy();
			this.searchTerms = oslcQueryParams.getSearchTerms();
			this.prefix = oslcQueryParams.getPrefix();
		} else {
			this.where = this.select = this.orderBy = this.searchTerms = this.prefix = null;
		}
		this.queryResource = createQueryResource(this.getCapabilityUrl());
		this.queryUrl = this.getQueryUrl();

		this.version = version;
	}

	OslcQuery(OslcQueryResult previousResult) {
		this(previousResult.getQuery(), previousResult.getNextPageUrl());
	}

	private OslcQuery(OslcQuery previousQuery, String nextPageUrl) {
		this(previousQuery.oslcClient, previousQuery.capabilityUrl, previousQuery.pageSize);
		this.queryUrl = nextPageUrl;
		this.queryResource = createQueryResource(nextPageUrl);
	}

	private WebTarget createQueryResource(final String capabilityUri) {
		WebTarget resource = oslcClient.getWebResource(capabilityUri);
		resource = applyPagination(resource);
		resource = applyOslcQueryParams(resource);
		return resource;
	}

	private WebTarget applyPagination(WebTarget resource) {
		WebTarget result = resource;
		if (pageSize > 0) {
			result = result.queryParam("oslc.paging", "true");
			result = result.queryParam("oslc.pageSize", pageSize);
		}
		return result;
	}

	private WebTarget applyOslcQueryParams(WebTarget resource) {
		WebTarget result = resource;
		if (this.where != null && !this.where.isEmpty()) {
			result = result.queryParam("oslc.where", this.where);
		}
		if (this.select != null && !this.select.isEmpty()) {
			result = result.queryParam("oslc.select", this.select);
		}
		if (this.orderBy != null && !this.orderBy.isEmpty()) {
			result = result.queryParam("oslc.orderBy", this.orderBy);
		}
		if (this.searchTerms != null && !this.searchTerms.isEmpty()) {
			result = result.queryParam("oslc.searchTerms", this.searchTerms);
		}
		if (this.prefix != null && !this.prefix.isEmpty()) {
			result = result.queryParam("oslc.prefix", this.prefix);
		}
		return result;
	}

	/**
	 * @return the number of entries to return for each page,
	 * 		if zero, the remote system's (or full query's) default is used
	 */
	public int getPageSize() {
		return pageSize;
	}

	/**
	 * @return the base query capability URL
	 */
	public String getCapabilityUrl() {
		return capabilityUrl;
	}

	/**
	 * @return the complete query URL
	 */
	public String getQueryUrl() {
		if (queryUrl == null) {
			queryUrl = queryResource.getUriBuilder().build(new Object[0]).toString();
		}
		return queryUrl;
	}

	public OslcQueryResult submit() {
		return new OslcQueryResult(this, getResponse());
	}

	Response getResponse() {
		return queryResource.request(OSLCConstants.CT_RDF)
				.header(OSLCConstants.OSLC_CORE_VERSION, version)
				.get();
	}

}
