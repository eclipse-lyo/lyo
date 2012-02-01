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
<%@ page contentType="application/x-oslc-compact+xml" language="java" pageEncoding="UTF-8" %>
<%@ page import="com.j2bugzilla.base.Bug" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<% 
Bug bug = (Bug)request.getAttribute("bug");
String baseUri = (String)request.getAttribute("baseUri");
String bugUri = (String)request.getAttribute("bugUri"); 
%>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
		 xmlns:dcterms="http://purl.org/dc/terms/" 
	     xmlns:oslc="http://open-services.net/ns/core#">
	     
	<oslc:Compact rdf:about="<%= bugUri %>">
		<dcterms:title><%= URLStrategy.getChangeRequestLinkLabel(bug.getID(), bug.getSummary()) %></dcterms:title>
		<oslc:shortTitle><%= "Bug"+bug.getID() %></oslc:shortTitle>
		<oslc:icon rdf:resource="<%= URLStrategy.getIconURL() %>" />
		
		<oslc:smallPreview>
			<oslc:Preview>
				<oslc:document rdf:resource="<%= URLStrategy.getChangeRequestURL(bug.getID()) + "&amp;preview=small" %>" />
				<oslc:hintWidth>500px</oslc:hintWidth>
				<oslc:hintHeight>160px</oslc:hintHeight>
			</oslc:Preview>
		</oslc:smallPreview>
		
		<oslc:largePreview>
			<oslc:Preview>
                <oslc:document rdf:resource="<%= URLStrategy.getChangeRequestURL(bug.getID()) + "&amp;preview=large" %>" />
				<oslc:hintWidth>500px</oslc:hintWidth>
				<oslc:hintHeight>500px</oslc:hintHeight>
			</oslc:Preview>
		</oslc:largePreview>
		
	</oslc:Compact>
	
</rdf:RDF>