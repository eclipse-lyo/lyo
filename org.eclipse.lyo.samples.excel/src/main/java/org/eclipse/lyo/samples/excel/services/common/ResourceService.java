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

import java.io.IOException;
import java.io.OutputStream;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.StreamingOutput;
import javax.ws.rs.core.UriInfo;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceSet;

//@Path(IConstants.SERVICE_SERVICES + "/{path:.+}")
@Path(IConstants.SERVICE_SERVICES + "/{projectId}/{excelfilename}/{id}")
public class ResourceService {

	@GET
	@Produces ({"application/rdf+xml", "application/xml"})
	public StreamingOutput getResource (
			@Context UriInfo uriInfo
			){
		String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
		ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
		ResourceSet resultSet = adapter.getResource(uriInfo.getAbsolutePath().toString());
        return new ResourceSetWriter(resultSet);
	}
	
	@GET
	@Produces ({"application/x-oslc-compact+xml"})
	public StreamingOutput doGet(@PathParam("projectId") String projectId, @PathParam("id") String id, @Context UriInfo uriInfo) {
		String title = "title"; //dummy
		String shortTitle = "ChangeRequest " + id;
		String resourceUri = uriInfo.getAbsolutePath().toString();

		String smUrl = uriInfo.getBaseUri() + "services/compact-rendering?uri=" + uriInfo.getAbsolutePath().toString() + "&amp;type=small&amp;projectId=" + projectId + "&amp;id=" + id;
		String lgUrl = uriInfo.getBaseUri() + "services/compact-rendering?uri=" + uriInfo.getAbsolutePath().toString() + "&amp;type=large&amp;projectId=" + projectId + "&amp;id=" + id;
		
        StringBuffer sb = new StringBuffer();
		 
		sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n");
		sb.append("<rdf:RDF \n");
		sb.append("   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" \n");
		sb.append("   xmlns:dcterms=\"http://purl.org/dc/terms/\" \n");
		sb.append("   xmlns:oslc=\"http://open-services.net/ns/core#\"> \n");
		sb.append(" <oslc:Compact \n");
		sb.append("   rdf:about=\"" + resourceUri + "\"> \n");
		sb.append("   <dcterms:title>" + title + "</dcterms:title> \n");
		sb.append("   <oslc:shortTitle>" + shortTitle + "</oslc:shortTitle> \n");
		sb.append("   <oslc:icon rdf:resource=\"../oslc.png\" /> \n");
		sb.append("   <oslc:smallPreview> \n");
		sb.append("      <oslc:Preview> \n");
		sb.append("         <oslc:document rdf:resource=\"" + smUrl + "\" /> \n");
		sb.append("         <oslc:hintWidth>500px</oslc:hintWidth> \n");
		sb.append("         <oslc:hintHeight>120px</oslc:hintHeight> \n");
		sb.append("      </oslc:Preview> \n");
		sb.append("   </oslc:smallPreview> \n");
		sb.append("   <oslc:largePreview> \n");
		sb.append("      <oslc:Preview> \n");
		sb.append("         <oslc:document rdf:resource=\"" + lgUrl + "\" /> \n");
		sb.append("         <oslc:hintWidth>500px</oslc:hintWidth> \n");
		sb.append("         <oslc:hintHeight>500px</oslc:hintHeight> \n");
		sb.append("      </oslc:Preview> \n");
		sb.append("   </oslc:largePreview> \n");
		sb.append(" </oslc:Compact> \n");
		sb.append("</rdf:RDF>");
		
		final String html = sb.toString();
		
		return new StreamingOutput() {
	            public void write(OutputStream output) throws IOException, WebApplicationException {
	                byte[] out = html.getBytes();
	                output.write(out);
	            }
	        };
	}
}
