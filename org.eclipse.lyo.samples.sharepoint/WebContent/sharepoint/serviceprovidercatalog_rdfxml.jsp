<?xml version="1.0" encoding="UTF-8"?>
<!--
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 -->
<%@ page contentType="application/rdf+xml" language="java"%>
<%@ page import="java.util.List" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.Library" %> 
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.URLStrategy"%>
<%
String bugzillaUri = (String) request.getAttribute("sharepointUri");
List<Library> libraries = (List<Library>)request.getAttribute("libraries");
response.setHeader("OSLC-Core-Version", "2.0");
%>
<rdf:RDF 
    xmlns:oslc="http://open-services.net/ns/core#" 
    xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">

    <oslc:ServiceProviderCatalog rdf:about="<%= URLStrategy.getServiceProviderCatalogURL() %>">
		<dcterms:title>OSLC Sharepoint Service Provider Catalog</dcterms:title>
		<dcterms:description>
		   Enables navigation to Service Provider for each Sharepoint Library.
		</dcterms:description>
		<oslc:domain rdf:resource="http://open-services.net/ns/cm#" />

		<dcterms:publisher>
			<oslc:Publisher>
				<dcterms:title>IBM Software Standards</dcterms:title>
				<dcterms:identifier>org.eclipse.lyo.samples.sharepoint.adapter</dcterms:identifier>
			</oslc:Publisher>
		</dcterms:publisher>
		
		<% for (Library l : libraries) { %>
		<oslc:serviceProvider>
            <oslc:ServiceProvider rdf:about="<%= l.getName() %>">
				<dcterms:title><%= l.getName() %></dcterms:title>
			    <oslc:details rdf:resource="<%= l.getUri() %>" />					
			</oslc:ServiceProvider>
		</oslc:serviceProvider>
		<% } %>

		<%--
		<oslc:oauthConfiguration>
			<oslc:OAuthConfiguration>
				<oslc:oauthRequestTokenURI rdf:resource="http://example.com/bugs/oauth-request-token" />
				<oslc:authorizationURI rdf:resource="http://example.com/bugs/oauth-authorization" />
				<oslc:oauthAccessTokenURI rdf:resource="http://example.com/bugs/oauth-access-token" />
			</oslc:OAuthConfiguration>
		</oslc:oauthConfiguration>
		 --%>

	</oslc:ServiceProviderCatalog>
</rdf:RDF>