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
package org.eclipse.lyo.samples.excel.services.common;

import java.net.URI;

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.samples.excel.common.ICmConstants;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.ResourceFactory;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;

@Path(ICmConstants.SERVICE_SERVICES + "/{projectId}/factory/changerequest")
public class CreationFactoryService {

	@POST
	//@Consumes ({"application/rdf+xml", "application/xml"})
	public Response doPost(@PathParam("projectId") String projectId, @Context UriInfo uriInfo, Model changeRequest) {
		
		ResIterator resIter = changeRequest.listSubjects();
		while(resIter.hasNext()){
			Resource resource = (Resource)resIter.nextResource();
			Property TITLE = ResourceFactory.createProperty("http://purl.org/dc/terms/", "title");
			Property STATUS = ResourceFactory.createProperty("http://open-services.net/ns/cm#", "status");
			Property CLOSED = ResourceFactory.createProperty("http://open-services.net/ns/cm#", "closed");
			System.out.println(resource);
			Statement title = resource.getProperty(TITLE);
			System.out.println(title.getObject());
			Statement status = resource.getProperty(STATUS);
			System.out.println(status.getObject());
			Statement closed = resource.getProperty(CLOSED);
			System.out.println(closed.getBoolean());
			
			String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
			ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
			
			
			StmtIterator stmtIter = resource.listProperties();
			while(stmtIter.hasNext()){
				Statement statement = stmtIter.nextStatement();
				
			}
		}

		// Return HTTP response with status code 201 Created 
		String uri = projectId + "/defects/";
		return Response.created(URI.create(uri)).build();
	}
}
