<!DOCTYPE html>

<%@ page language="java" contentType="text/html; UTF-8"
	pageEncoding="UTF-8"%>
<%@ page isELIgnored ="false" %> 
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<html lang="en">

<head>
<meta charset="utf-8">
<title>Log In</title>
<link type="text/css" href="<%=request.getContextPath()%>/oauth/theme.css" rel="stylesheet" ></link>
<script
	data-dojo-config="async: true"
    type="text/javascript"
    src="http://ajax.googleapis.com/ajax/libs/dojo/1.7.1/dojo/dojo.js">
</script>
<script type="text/javascript" src="<%=request.getContextPath()%>/oauth/login.js"></script>
</head>

<body>
	<div id="content">
		<form id="loginForm">
		
			<div class="message">Another application, <c:out value="${consumerName}">[Unknwon]</c:out>,
			is requesting access to your <c:out value="${applicationName}">application</c:out> data. Enter your
			username and password to continue or click cancel to exit.</div>
			
			<div id="error" class="error" style="display: hidden;"></div>
			<input type="hidden" name="requestToken" value="<c:out value="${requestToken}"/>">
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
				<input type="submit" value="OK">
				<button type="button" id="cancel">Cancel</button>
			</div>
		</form>
	</div>
</body>

</html>
