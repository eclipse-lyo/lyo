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
		<%-- Creative Commons Image: http://openclipart.org/detail/211/shiny-key-by-tiothy --%>
		<img src="<%=request.getContextPath()%>/oauth/key.png" style="float: right;">
		<h1>Connect to <c:out value="${applicationName}">Your Application</c:out></h1>

		<form id="loginForm">
		
			<div><b><c:out value="${consumerName}">Another application</c:out></b>
			is requesting access to your <b><c:out value="${applicationName}">application</c:out></b> data. Enter your
			username and password to continue or click cancel to exit.</div>

			<c:if test="${not callbackConfirmed}">
				<p>
					Only continue if you initiated this request directly from
					<c:out value="${consumerName}">the consumer's website</c:out>.
				</p>
			</c:if>

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
