<!DOCTYPE html>
<%--
 Copyright (c) 2011, 2012 IBM Corporation.

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 
 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.
 
 Contributors:
 
    Sam Padgett	  	- initial API and implementation
--%>
<%@ page language="java" contentType="text/html; UTF-8"
	pageEncoding="UTF-8"%>
<%@ page isELIgnored="false"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>


<html lang="en">

<head>
<meta charset="utf-8">
<title>Manage OAuth Consumers</title>
<link type="text/css"
	href="<%=request.getContextPath()%>/oauth/stylesheets/theme.css" rel="stylesheet"></link>
<link type="text/css"
	href="<%=request.getContextPath()%>/oauth/stylesheets/admin.css" rel="stylesheet"></link>
<link rel="stylesheet" type="text/css" href="<%=request.getContextPath()%>/theme/jazz/view.css">
<jsp:include page="/oauth/common.jsp"/>
<script data-dojo-config="async: true" type="text/javascript"
	src="//ajax.googleapis.com/ajax/libs/dojo/1.7.1/dojo/dojo.js">
</script>
<script type="text/javascript" src="<%=request.getContextPath()%>/oauth/scripts/manage.js"></script>
</head>

<body>
	<h1><c:out value="${applicationName}"/> - Manage OAuth Consumers</h1>

	<div class="content">

		<div id="error" class="error" style="display: none;"></div>

		<div class="sectionHeader">
			<span class="sectionTitle">Pending</span>
		</div>

		<p id="noPendingMessage" class="message" style="display: none;">No pending consumers.</p>
		<table id="pendingTable" class="consumers" style="display: none;">
			<thead>
				<tr>
					<th class="consumerName">Name</th>
					<th class="consumerKey">Key</th>
					<th class="trusted">Trusted</th>
					<th class="actions">Actions</th>
				</tr>
			</thead>
			<tbody></tbody>
		</table>

		<div class="sectionHeader">
			<span class="sectionTitle">Active</span>
		</div>

		<p id="noApprovedMessage" class="message" style="display: none;">No approved consumers.</p>
		<table id="approvedTable" class="consumers" style="display: none;">
			<thead>
				<tr>
					<th class="consumerName">Name</th>
					<th class="consumerKey">Key</th>
					<th class="trusted">Trusted</th>
					<th class="actions">Actions</th>
				</tr>
			</thead>
			<tbody></tbody>
		</table>
	</div>
</body>

</html>
