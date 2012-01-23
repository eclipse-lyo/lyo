/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
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
package org.eclipse.lyo.samples.excel.changerequest;

import java.net.URI;

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.samples.excel.common.ICmConstants;


@Path(ICmConstants.SERVICE_SERVICES + "/{projectId}/generate/changerequest")
public class ChangeRequestGeneratorService  {

	@POST
	//@Consumes ({"application/rdf+xml", "application/xml"})
	public Response doPost(@PathParam("projectId") String projectId, @Context UriInfo uriInfo, MultivaluedMap<String, String> formData) {
		int count = Integer.parseInt(formData.getFirst("count"));
		
		try{
			String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
			ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
			adapter.loadRepository();
			adapter.generateDefaultContents(count);
			
		} catch( Exception e ) {
            e.printStackTrace();
		}
		// Return HTTP response with status code 201 Created 
		String uri = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES + "/" + projectId + "/list";
		return Response.seeOther(URI.create(uri)).build();
	}
	

}
