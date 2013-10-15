/*******************************************************************************
 * Copyright (c) 2011,2013 IBM Corporation.
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
 *     Masaki Wakao 
 *     Yoshio Horiuchi 
 *     Kohji Ohsawa 
 *******************************************************************************/
package org.eclipse.lyo.samples.excel.services.common;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.StreamingOutput;
import javax.ws.rs.core.UriInfo;


import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceSet;

@Path(IConstants.SERVICE_SERVICES + "/{projectId}/query/{type}")
public class QueryService {

	@GET
	@Produces ({"application/rdf+xml", "application/xml"})
	public StreamingOutput getQueryResult(
			@Context UriInfo uriInfo, 
			@PathParam("projectId") String projectId, 
			@QueryParam("oslc.prefix") String oslcPrefix,				// OSLC Core
			@QueryParam("oslc.where") String oslcWhere,					// OSLC Core
			@QueryParam("oslc.select") String oslcSelect,				// OSLC Core
			@QueryParam("oslc.properties") String oslcProperties,		// OSLC Core
			@QueryParam("oslc_cm.properties") String oslcCmProperties	// OSLC CM 1.0
			){
		String select = oslcSelect; 
		if(select == null){
			select = oslcCmProperties; 
			if(select == null){
				if(select == null || select.length() == 0){
					select = "dc:identifier,dc:title"; // default select
				}
			}
		}
		
		String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
		ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
//		Model model = adapter.query(projectId, oslcPrefix, select);
		ResourceSet resultSet = adapter.query(uriInfo.getAbsolutePath().toString(), projectId, oslcPrefix, select, oslcWhere, null, null);
        return new ResourceSetWriter(resultSet);
	}

}
