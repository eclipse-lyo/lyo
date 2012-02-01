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
<%@ page import="java.util.List" %>
<%@ page import="com.j2bugzilla.base.Bug" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%
	String bugzillaUri = (String) request.getAttribute("bugzillaUri");
    Product product = (Product)request.getAttribute("product");
	List<String> operatingSystems = (List<String>)request.getAttribute("operatingSystems");
	List<String> components = (List<String>)request.getAttribute("components");
	List<String> platforms  = (List<String>)request.getAttribute("platforms");
	List<String> versions   = (List<String>)request.getAttribute("versions");
%>
<oslc:ResourceShape xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:dc="http://purl.org/dc/terms/" 
	xmlns:oslc="http://open-services.net/ns/core#"
	rdf:about="http://example.com/resource/ChangeRequestShape4Reporting">

	<oslc:describes rdf:resource="http://www.bugzilla.org/rdf#QueryResponse" />

	<dc:title>Resource Shape for Query</dc:title>

	<oslc:property>
		<oslc:Property>
			<oslc:name>member</oslc:name>
			<oslc:propertyDefinition rdf:resource="http://www.w3.org/2000/01/rdf-schema#member" />
			<oslc:valueType rdf:resource="http://open-services.net/ns/core#AnyResource" />
            <oslc:valueShape rdf:resource="<%= URLStrategy.getQueryChangeRequestShapeURL(product.getId()) %>" />
			<oslc:occurs rdf:resource="http://open-services.net/ns/core#Zero-or-many" />
            <oslc:isMemberProperty>true</oslc:isMemberProperty>
		</oslc:Property>
	</oslc:property>

</oslc:ResourceShape>
