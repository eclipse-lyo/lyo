<?xml version="1.0" encoding="UTF-8"?>
<%--
 Copyright (c) 2011 IBM Corporation.

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 
 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.
 
 Contributors:
 
    IBM Corporation - initial API and implementation
--%>
<%@ page contentType="application/x-oslc-compact+xml" language="java"%>
<%@ page import="jbugz.base.Bug" %>
<%
	Bug bug = (Bug) request.getAttribute("bug");
	String bugUri = (String) request.getAttribute("bugUri");
%>
<rdf:RDF 
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:oslc="http://open-services.net/ns/core#"
	xmlns:oslc_cm="http://open-services.net/ns/cm#"
	xmlns:bugz="http://www.bugzilla.org/rdf#"
	xmlns:foaf="http://http://xmlns.com/foaf/0.1/">

	<oslc_cm:ChangeRequest rdf:about="<%= bugUri %>">	
		<rdf:type rdf:resource="http://open-services.net/ns/cm#ChangeRequest" />

		<dcterms:title><%=      bug.getSummary() %></dcterms:title>
		<dcterms:identifier><%= bug.getID() %></dcterms:identifier>
		<dcterms:contributor>
		  <foaf:Person>
			<foaf:name><%=      bug.getInternalState().get("assigned_to") %></foaf:name>
		  </foaf:Person> 
		</dcterms:contributor>

		<oslc_cm:status><%=     bug.getStatus() %></oslc_cm:status>

		<dcterms:modified><%=   bug.getInternalState().get("last_change_time") %></dcterms:modified>
		<dcterms:created><%=    bug.getInternalState().get("creation_time") %></dcterms:created>

		<bugz:product><%=       bug.getProduct() %></bugz:product>		
		<bugz:component><%=     bug.getComponent() %></bugz:component>		
		<bugz:version><%=       bug.getVersion() %></bugz:version>		
		<bugz:priority><%=      bug.getPriority().name() %></bugz:priority>		
<%-- 		<bugz:platform><%=      bug.getPlatform() %></bugz:platform>		
		<bugz:opsys><%=         bug.getOpsys() %></bugz:opsys>		
 --%>
	</oslc_cm:ChangeRequest>

</rdf:RDF>

