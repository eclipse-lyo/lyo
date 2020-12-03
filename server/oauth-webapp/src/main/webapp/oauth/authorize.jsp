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
<title>Connect to <c:out value="${applicationName}">Your Application</c:out></title>
<link type="text/css" href="<%=request.getContextPath()%>/oauth/stylesheets/theme.css" rel="stylesheet" ></link>
<jsp:include page="/oauth/common.jsp"/>
<script
	data-dojo-config="async: true"
    type="text/javascript"
    src="//ajax.googleapis.com/ajax/libs/dojo/1.7.1/dojo/dojo.js">
</script>
<script type="text/javascript" src="<%=request.getContextPath()%>/oauth/scripts/authorize.js"></script>
</head>

<body>
	<div id="content">
		<form id="authorizationForm">
			<h1>Authorize Application</h1>
		
			<p><b><c:out value="${consumerName}">Another application</c:out></b>
			is requesting access to your <b><c:out value="${applicationName}">application</c:out></b> data. Allow?</p>

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
				<input type="submit" value="Allow">
				<button type="button" id="deny">Deny</button>
			</div>
		</form>
	</div>
	
	<script>
	// TODO: Factor out common code from login.js.
	require([ "dojo/dom", "dojo/on", "dojo/_base/event", "dojo/_base/xhr", "dojo/ready" ],
			function(dom, on, event, xhr, ready) {
		function showError(message) {
			var errorNode = dom.byId('error');
			if (message) {
				domConstruct.empty(errorNode);
				errorNode.appendChild(document.createTextNode(message));
			} else {
				errorNode.innerHTML = 'An error occurred.';
			}
			errorNode.style.display = 'block';
		}
		
		function returnToConsumer() {
			var callback = dom.byId('callback').value;
			if (callback) {
				window.location = callback;
			} else {
				dom.byId('content').innerHTML =
					'<div class="message">Request authorized. Close the browser window to continue.</div>';
			}
		}
		
		function cancel() {
			dom.byId('content').innerHTML =
				'<div class="message">Access denied. Close the browser window to continue.</div>';
		}
	
		ready(function() {
			on(dom.byId('authorizationForm'), 'submit', function(e) {
				event.stop(e);
				xhr.post({
					url : 'internal/approveToken',
					form : 'authorizationForm',
					headers : {
						'X-CSRF-Prevent': lyoOAuthConfig.csrfPrevent
					},
					load : function() {
						returnToConsumer();
					},
					error : function(error, ioArgs) {
						showError(ioArgs.xhr.responseText);
					}
				});
			});
	
			on(dom.byId('deny'), 'click', function(e) {
				event.stop(e);
				cancel();
			});
		});
	});
	</script>
</body>

</html>
