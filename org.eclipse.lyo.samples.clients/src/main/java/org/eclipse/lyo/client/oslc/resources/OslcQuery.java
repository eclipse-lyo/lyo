/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Sean Kennedy     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.Resource;
import org.eclipse.lyo.client.oslc.OslcClient;

/**
 * Represents an OSLC query (HTTP GET) request to be made of a remote system.
 * 
 * Immutable.
 */
//TODO: Support additional (possibly arbitrary) query parameters
//		if we need to allow arbitrary query parameters, 
//		or have too many possible parameters for ease of use in constructors,
//		decide on some way to allow users to build the parameter list with
//		multiple statements.
//	Possible solution: contained static class that has all available parameters
//	(perhaps with support for arbitrary parameters) that is passed on the 
//	constructor and held as a final variable.
public class OslcQuery {

	private final OslcClient oslcClient;
	
	private final String capabilityUrl;
	
	private String queryUrl;
	
	private final int pageSize;
	
	private final Resource queryResource;
	
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
	 * Create an OSLC query that uses the given page size
	 * 
	 * @param oslcClient the authenticated OSLC client
	 * @param capabilityUrl the URL that is the base
	 * @param pageSize the number of results to include on each page (OslcQueryResult)
	 */
	public OslcQuery(OslcClient oslcClient, String capabilityUrl, int pageSize) {
		this.oslcClient = oslcClient;
		this.capabilityUrl = capabilityUrl;
		this.pageSize = (pageSize < 1) ? 0 : pageSize;
		this.queryResource = createQueryResource();
		this.queryUrl = null;
	}
	
	OslcQuery(OslcQueryResult previousResult) {
		this(previousResult.getQuery(), previousResult.getNextPageUrl());
	}
	
	private OslcQuery(OslcQuery previousQuery, String nextPageUrl) {
		this(previousQuery.oslcClient, previousQuery.capabilityUrl, previousQuery.pageSize);
		this.queryUrl = nextPageUrl;
		this.queryResource.uri(nextPageUrl);
	}
	
	private Resource createQueryResource() {
		Resource resource = oslcClient.getRemoteResource(this);
		resource.accept("application/rdf+xml");
		resource.header("Oslc-Core-Version","2.0");
		applyPagination(resource);
		return resource;
	}
	
	private void applyPagination(Resource resource) {
		if (pageSize > 0) {
			resource.queryParam("oslc.paging", "true");
			resource.queryParam("oslc.pageSize", pageSize);
		}
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
	
	ClientResponse getResponse() {
		return queryResource.get();
	}

}
