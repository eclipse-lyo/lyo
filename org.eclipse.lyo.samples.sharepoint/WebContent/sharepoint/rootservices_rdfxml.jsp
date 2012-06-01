<?xml version="1.0" encoding="UTF-8"?>
<%@ page contentType="application/rdf+xml;charset=UTF-8" language="java"%>
<%
String baseUri = (String) request.getAttribute("baseUri");
%>
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
<!-- Jazz Root Services, see:
	https://jazz.net/wiki/bin/view/Main/RootServicesSpec
	https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2
 -->
<rdf:Description rdf:about="<%= baseUri + "/rootservices" %>"
    xmlns:oslc_cm="http://open-services.net/xmlns/cm/1.0/"
    xmlns:dcterms="http://purl.org/dc/terms/"
    xmlns:jfs="http://jazz.net/xmlns/prod/jazz/jfs/1.0/" 
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">

	<dcterms:title>OSLC Sharepoint Adapter/Jazz Root Services</dcterms:title>
	<oslc_cm:cmServiceProviders rdf:resource="<%= baseUri + "/catalog" %>" />	
	<jfs:oauthAccessTokenUrl rdf:resource="<%= baseUri + "/access_token" %>" />
	<jfs:oauthRealmName>Sharepoint</jfs:oauthRealmName>
	<jfs:oauthDomain><%= baseUri %></jfs:oauthDomain>
	<jfs:oauthUserAuthorizationUrl rdf:resource="<%= baseUri + "/authorize" %>" />
	<jfs:oauthRequestConsumerKeyUrl rdf:resource="<%= baseUri + "/consumer_key" %>" />	
</rdf:Description>
