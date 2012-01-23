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

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriInfo;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.samples.excel.common.ICmConstants;

@Path(ICmConstants.SERVICE_SERVICES + "/{projectId}/creator")
public class ChangeRequestCreatorService {
	
	@POST
	//@Consumes ({"application/rdf+xml", "application/xml"})
	//@Consumes ({"text/xml"})
	public void doPost(@PathParam("projectId") String projectId, @Context UriInfo uriInfo, MultivaluedMap<String, String> formData) {
		String title = formData.getFirst("title");
		String description = formData.getFirst("description");
		String status = formData.getFirst("status");
		
		if(description == null){
			description = "";
		}
		
		try{
			ChangeRequestDto dto = new ChangeRequestDto();
			dto.setTitle(title);
			dto.setDescription(description);
			dto.setStatus(status);
			
			String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
			ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
			adapter.addChangeRequest(dto);			
		} catch( Exception e) {
			e.printStackTrace();
		}
//		String uri = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES + "/" + projectId + "/list";
//		return Response.seeOther(URI.create(uri)).build();
	}
}
