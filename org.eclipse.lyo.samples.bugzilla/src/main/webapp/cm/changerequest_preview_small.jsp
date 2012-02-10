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
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt"%>
<%
Bug bug = (Bug)request.getAttribute("bug");
String bugzillaUri = (String) request.getAttribute("bugzillaUri");
String assignee = (String)bug.getParameterMap().get("assigned_to");
%>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
<title>Change Request: <c:out value="${bug.summary}"/> (<c:out value="${bug.ID}"/>)</title>
<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%= bugzillaUri %>/skins/contrib/Dusk/global.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%= bugzillaUri %>/skins/contrib/Dusk/index.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%= bugzillaUri %>/skins/custom/global.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/custom/index.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="<%= bugzillaUri %>/images/favicon.ico">
<style type="text/css">
body {
	background: #FFFFFF;
	padding: 0;
}

td {
	padding-right: 5px;
	min-width: 175px;
}

th {
	padding-right: 5px;
	text-align: right;
}
</style>
</head>
<body>
	<div id="bugzilla-body">
<table class="edit_form">
	<tr>
		<th>Status:</th>
		<td><c:out value="${bug.status}"/></td>
		<th>Product:</th>
		<td><c:out value="${bug.product}"/></td>
	</tr>
	
	<tr>
		<th>Assignee:</th>
		<td><c:out value="<%= bug.getParameterMap().get("assigned_to") %>">Unassigned</c:out></td>
		<th>Component:</th>
		<td><c:out value="${bug.component}"/></td>
	</tr>
	
	<tr>
		<th>Priority:</th>
		<td><c:out value="${bug.priority}"/></td>
		<th>Version:</th>
		<td><c:out value="${bug.version}"/></td>
	</tr>
	
	<tr>
		<th>Reported:</th>
		<td><fmt:formatDate value="<%=(Date)bug.getParameterMap().get("last_change_time")%>" type="date"
				dateStyle="default" /></td>
		<th>Modified:</th>
		<td><fmt:formatDate value="<%=(Date)bug.getParameterMap().get("creation_time")%>" type="date"
				dateStyle="default" /></td>
	</tr>
</table>
</div>
</body>
</html>