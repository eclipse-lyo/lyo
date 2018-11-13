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
<%@ page isELIgnored ="false" %> 
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<html lang="en">

<head>
<meta charset="utf-8">
<title><c:out value="${applicationName}">Administer</c:out> - Log In</title>
<link type="text/css" href="<%=request.getContextPath()%>/oauth/stylesheets/theme.css" rel="stylesheet" ></link>
<jsp:include page="/oauth/common.jsp"/>
<script
	data-dojo-config="async: true"
    type="text/javascript"
    src="//ajax.googleapis.com/ajax/libs/dojo/1.7.1/dojo/dojo.js">
</script>
<script type="text/javascript" src="<%=request.getContextPath()%>/oauth/scripts/adminLogin.js"></script>
</head>

<body>
	<div id="content">
		<%-- Creative Commons Image: http://openclipart.org/detail/211/shiny-key-by-tiothy --%>
		<img src="<%=request.getContextPath()%>/oauth/images/key.png" style="float: right;">
		<h1><c:out value="${applicationName}">Administer</c:out> - Log In</h1>

		<form id="loginForm">
		
			<div>You must log in as an OAuth administrator to continue.</div>

			<div id="error" class="error" style="display: hidden;"></div>
			<input type="hidden" id="callback" value="<c:out value="${callback}"/>">
			
			<div>
				<label for="id">Username:</label>
			</div>
			<div>
			 	<input id="id" name="id" type="text" class="textField" required autofocus></input>
				<script type="text/javascript">
					// If no native HTML5 autofocus support, focus the ID field using JavaScript.
		    		if (!("autofocus" in document.createElement("input"))) {
		      			document.getElementById("id").focus();
		    		}
		  		</script>
			</div>
	
			<div>
				<label for="password">Password:</label>
			</div>
				<input id="password" name="password" type="password" class="textField"></input>
			<div>
			</div>
			<div>
				<input type="submit" value="Continue">
			</div>
		</form>
	</div>
</body>

</html>
