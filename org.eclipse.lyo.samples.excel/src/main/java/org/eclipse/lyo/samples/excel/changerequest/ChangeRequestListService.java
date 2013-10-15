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
package org.eclipse.lyo.samples.excel.changerequest;

import java.io.IOException;
import java.io.OutputStream;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.StreamingOutput;
import javax.ws.rs.core.UriInfo;

import org.eclipse.lyo.rio.core.IConstants;
import org.eclipse.lyo.samples.excel.adapter.common.AdapterRegistry;
import org.eclipse.lyo.samples.excel.adapter.common.ResourceAdapter;

import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;

@Path(IConstants.SERVICE_SERVICES + "/{projectId}/list")
public class ChangeRequestListService  {
	
	@GET
	public StreamingOutput doGet(@PathParam("projectId") String projectId, @Context UriInfo uriInfo) {
		String baseUrl = uriInfo.getBaseUri().toString() + IConstants.SERVICE_SERVICES;
		ResourceAdapter adapter = AdapterRegistry.getAdapter(baseUrl);
		
		String query = 
			"PREFIX dcterms:<http://purl.org/dc/terms/>\n" +
			"SELECT ?uri ?title ?identifier\n" +
			"WHERE {\n" +
			"  ?uri dcterms:title ?title.\n" +
			"  ?uri dcterms:identifier ?identifier\n" +
			"}\n";

		ResultSet resultSet = adapter.executeSparql(projectId, query);
		
		String contextRoot = baseUrl.substring(0, baseUrl.lastIndexOf("/rest/services"));
		final String html = buildHtml(resultSet, contextRoot);
		
		return new StreamingOutput() {
	            public void write(OutputStream output) throws IOException, WebApplicationException {
	                byte[] out = html.getBytes(IConstants.TEXT_ENCODING);
	                output.write(out);
	            }
	        };
	}

	private String buildHtml(ResultSet resultSet, String contextRoot) {
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
		sb.append("<link rel=\"SHORTCUT ICON\" href=\"../oslc.png\">");
		sb.append("<title>RIO OSLC CM Resource Listing</title>");
		sb.append("<script type=\"text/javascript\" src=\"" + contextRoot + "/smallpreview.js\">");
		sb.append("</script>");
		sb.append("</head>");
		sb.append("<body>");
		sb.append("<h3>RIO Change Request Listing</h3>");
		sb.append("<table>");

		while (resultSet.hasNext()) {
	    	QuerySolution qs = resultSet.nextSolution();
	    	String uri = qs.get("uri").toString();
	    	String title = qs.get("title").toString();
	    	String id = qs.get("identifier").toString();
	    	
	    	sb.append("<tr><td>");
	    	sb.append("<a href=\"" + uri + "\" onmouseover=\"hover('" + uri + "','d" + id + "');\"" + " onmouseout=\"closeHover();\">" + uri + "</a>    "); 
	    	sb.append("<b>" + title + "(" + id + ")</b>");
	    	sb.append("	<div id=\"d" + id + "\"></div>");	
	    	sb.append("</tr></td>");
		}		

		sb.append("</table>");
		sb.append("</body>");
		sb.append("</html>");

		return sb.toString();
	}

}









