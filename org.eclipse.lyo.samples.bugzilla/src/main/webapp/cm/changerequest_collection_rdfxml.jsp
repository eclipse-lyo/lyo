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
<%@ page contentType="application/rdf+xml" language="java" %>
<%@ page import="jbugz.base.Bug" %>
<%@ page import="java.util.List" %>
<%
	List<Bug> bugs = (List<Bug>)request.getAttribute("results"); 
	String queryUri = (String)request.getAttribute("queryUri");
	String nextPageUri = (String)request.getAttribute("nextPageUri");
%>
<rdf:RDF 
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
	xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:foaf="http://http://xmlns.com/foaf/0.1/"
    xmlns:oslc="http://open-services.net/ns/core#"
	xmlns:oslc_cm="http://open-services.net/ns/cm#"
	xmlns:bugz="http://www.bugzilla.org/rdf#" 
	>
	
	<oslc:ResponseInfo rdf:about="<%= queryUri %>">
        <dcterms:title>Query Results</dcterms:title>
        <% if (nextPageUri != null) { %><oslc:nextPage><%= nextPageUri %></oslc:nextPage><% } %>
    </oslc:ResponseInfo>

    <bugz:QueryResponse rdf:about="<%= queryUri %>">
 
    <% for (Bug bug : bugs) { %>
      <rdfs:member>
		<oslc_cm:ChangeRequest rdf:about="<%= bug.getInternalState().get("oslc_uri") %>">	
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
		</oslc_cm:ChangeRequest>
      </rdfs:member>
	<% } %>
	
    </bugz:QueryResponse> 
    
</rdf:RDF>

