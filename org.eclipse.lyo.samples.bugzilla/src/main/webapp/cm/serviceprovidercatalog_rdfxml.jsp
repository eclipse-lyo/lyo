<?xml version="1.0" encoding="UTF-8"?>
<%--
 Copyright (c) 2011, 2012 IBM Corporation.

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 
 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.
 
 Contributors:
 
    IBM Corporation - initial API and implementation
--%>
<%@ page contentType="application/rdf+xml" language="java" pageEncoding="UTF-8" %>
<%@ page import="java.util.List" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%
String bugzillaUri = (String) request.getAttribute("bugzillaUri");
List<Product> products = (List<Product>)request.getAttribute("products");
response.setHeader("OSLC-Core-Version", "2.0");
%>
<rdf:RDF 
    xmlns:oslc="http://open-services.net/ns/core#" 
    xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">

    <oslc:ServiceProviderCatalog rdf:about="<%= URLStrategy.getServiceProviderCatalogURL() %>">
		<dcterms:title>OSLC-CM Adapter/Bugzilla Service Provider Catalog</dcterms:title>
		<dcterms:description>
		   Enables navigation to Service Provider for each Product against which bugs may be reported.
		</dcterms:description>
		<oslc:domain rdf:resource="http://open-services.net/ns/cm#" />

		<dcterms:publisher>
			<oslc:Publisher>
				<dcterms:title>OSLC Tools Project</dcterms:title>
				<dcterms:identifier>org.eclipse.lyo.samples.bugzilla.test</dcterms:identifier>
				<oslc:icon rdf:resource="<%= bugzillaUri %>/images/favicon.ico" />
			</oslc:Publisher>
		</dcterms:publisher>
		
		<% for (Product p : products) { %>
		<oslc:serviceProvider>
            <oslc:ServiceProvider rdf:about="<%= URLStrategy.getServiceProviderURL(p.getId()) %>">
				<dcterms:title><%= p.getName() %></dcterms:title>
			    <oslc:details rdf:resource="<%= URLStrategy.getServiceProviderURL(p.getId()) %>" />					
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