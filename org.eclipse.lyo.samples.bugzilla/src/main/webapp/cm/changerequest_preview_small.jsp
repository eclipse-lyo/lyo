<!DOCTYPE html>
<!--
 Copyright (c) 2011, 2012 IBM Corporation.

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 
 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.
 
 Contributors:
 
    IBM Corporation - initial API and implementation
 -->
<%@ page contentType="text/html" language="java" pageEncoding="UTF-8" %>
<%@ page import="java.net.*,java.util.*,java.text.SimpleDateFormat" %>
<%@ page import="com.j2bugzilla.base.Bug" %>
<%
Bug bug = (Bug)request.getAttribute("bug");
String bugzillaUri = (String) request.getAttribute("bugzillaUri");
String bugUri = (String)request.getAttribute("bugUri");

String title = bug.getSummary();
Date createdDate = (Date)bug.getParameterMap().get("creation_time"); 
SimpleDateFormat formatter = new SimpleDateFormat();
String created = formatter.format(createdDate);
Date modifiedDate = (Date)bug.getParameterMap().get("last_change_time");
String modified = formatter.format(modifiedDate);
String identifier = bug.getID()+""; 
String description = "None";
String assignee = (String)bug.getParameterMap().get("assigned_to");
%>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
<title>Change Request: <%=title %> (<%=identifier %>)</title>
<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%= bugzillaUri %>/skins/contrib/Dusk/global.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%= bugzillaUri %>/skins/contrib/Dusk/index.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%= bugzillaUri %>/skins/custom/global.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/custom/index.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="<%= bugzillaUri %>/images/favicon.ico">
</head>
<body>
<b>Bugzilla Bug #<%= bug.getID() %></b>
<table>
	<tr>
		<td>Summary:</td>
		<td><%= title %></td>
	</tr>
	<tr>
		<td>Identifier:</td>
		<td><%= identifier %></td>
	</tr>
	<tr>
		<td>URI:</td>
		<td><%= bugUri %></td>
	</tr>
	<tr>
		<td>Created:</td>
		<td><%= created %></td>
	</tr>
	<tr>
		<td>Modified:</td>
		<td><%= modified %></td>
	</tr>
</table>
</body>
</html>