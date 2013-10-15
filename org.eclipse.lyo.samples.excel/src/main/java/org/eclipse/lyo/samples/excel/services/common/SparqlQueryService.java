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
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Iterator;

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.StreamingOutput;
import javax.ws.rs.core.UriInfo;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;

import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;

@Path(IConstants.SERVICE_SERVICES + "/{projectId}/sparql")
public class SparqlQueryService  {

	@POST
	public StreamingOutput doPost(@PathParam("projectId") String projectId, @Context UriInfo uriInfo, MultivaluedMap<String, String> formData){
		String queryExp = null;
		try{	
			queryExp = URLDecoder.decode( formData.getFirst("queryExp"), IConstants.TEXT_ENCODING );
		}catch(UnsupportedEncodingException e){
			e.printStackTrace();
		}
		String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
		ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
		ResultSet resultSet = adapter.executeSparql(projectId, queryExp);
		
		final String html = buildHtml(resultSet);
		
		return new StreamingOutput() {
	            public void write(OutputStream output) throws IOException, WebApplicationException {
	                byte[] out = html.getBytes(IConstants.TEXT_ENCODING);
	                output.write(out);
	            }
	        };
	}

	private String buildHtml(ResultSet resultSet) {
		StringBuffer sb = new StringBuffer();
		sb.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">");
		sb.append("<!--");
		sb.append("   Copyright (c) 2011 IBM Corporation.");
		sb.append("   All rights reserved. This program and the accompanying materials");
		sb.append("   are made available under the terms of the Eclipse Distribution License v. 1.0 ");
		sb.append("   which accompanies this distribution, and is available at ");
		sb.append("   http://www.eclipse.org/org/documents/edl-v10.php.");
		sb.append("");    
		sb.append("   Contributors:");
		sb.append("      Kohji Ohsawa ");
		sb.append(" -->");		
		sb.append("<html>");
		sb.append("<head>");
		sb.append("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=" + IConstants.TEXT_ENCODING + "\">");
		sb.append("<link rel=\"SHORTCUT ICON\" href=\"oslc.png\">");
		sb.append("<title>RDF Store SPARQL</title>");
		sb.append("</head>");
		sb.append("<body>");
		sb.append("<h3>Results</h3>");
		sb.append("<table border=1>");
		
		boolean needsHeaderRow = true;
		ArrayList<String> columns = new ArrayList<String>();
		
		while (resultSet.hasNext()) {
        	QuerySolution qs = resultSet.nextSolution();
        	Iterator<String> headers = qs.varNames();
        	if(needsHeaderRow){
        		sb.append("<tr>");
        		while(headers.hasNext()) {
	        		String header = headers.next();
	        		sb.append("<th>" + header + "</th>");
	        		columns.add(header);
	        	}
	        	needsHeaderRow = false;
	        	sb.append("</tr>");
        	}
        	sb.append("<tr>");
        	for(String column: columns){
        		sb.append("<td>" + qs.get(column).toString() + "</td>");
        	}
        	sb.append("</tr>");
        	
		}
		
		sb.append("</table>");
		sb.append("</body>");
		sb.append("</html>");
		
		return sb.toString();
	}

}
