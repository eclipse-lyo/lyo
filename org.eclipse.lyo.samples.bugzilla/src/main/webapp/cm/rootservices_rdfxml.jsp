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
<%@ page contentType="application/rdf+xml" language="java" %>
<%@ page pageEncoding="UTF-8" %>
<%
String baseUri = (String) request.getAttribute("baseUri");
%>
<!-- Jazz Root Services, see:
	https://jazz.net/wiki/bin/view/Main/RootServicesSpec
	https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2
 -->
<rdf:Description rdf:about="<%= baseUri + "/rootservices" %>"
    xmlns:oslc_cm="http://open-services.net/xmlns/cm/1.0/"
    xmlns:dcterms="http://purl.org/dc/terms/"
    xmlns:jfs="http://jazz.net/xmlns/prod/jazz/jfs/1.0/" 
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">

	<dcterms:title>OSLC-CM Adapter/Bugzilla Jazz Root Services</dcterms:title>
	<oslc_cm:cmServiceProviders rdf:resource="<%= baseUri + "/catalog" %>" />
	<jfs:oauthRealmName>Bugzilla</jfs:oauthRealmName>
	<jfs:oauthDomain><%= baseUri %></jfs:oauthDomain>
	<jfs:oauthRequestConsumerKeyUrl rdf:resource="<%= baseUri + "/services/oauth/requestKey" %>" />
	<jfs:oauthApprovalModuleUrl rdf:resource="<%= baseUri + "/services/oauth/approveKey" %>" />
	<jfs:oauthRequestTokenUrl rdf:resource="<%= baseUri + "/services/oauth/requestToken" %>"/>
	<jfs:oauthUserAuthorizationUrl rdf:resource="<%= baseUri + "/services/oauth/authorize" %>" />
	<jfs:oauthAccessTokenUrl rdf:resource="<%= baseUri + "/services/oauth/accessToken" %>"/>	
</rdf:Description>
