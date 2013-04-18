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
<title><c:out value="${applicationName}">OAuth</c:out> Consumer
	Approved</title>
<link type="text/css"
	href="<%=request.getContextPath()%>/oauth/stylesheets/theme.css" rel="stylesheet"></link>
</head>
<body>
	<p>
		<b><c:out value="${applicationName}">OAuth</c:out></b>
		consumer
		<b><c:out value="${consumerName}">
			<c:out value="${consumerKey}" />
		</c:out></b>
		is already approved.
	</p>
	<p>
		<a href="admin" target="_blank">Manage Consumers</a>
	</p>
</body>
</html>
