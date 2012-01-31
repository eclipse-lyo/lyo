<!DOCTYPE html>

<%@ page language="java" contentType="text/html; UTF-8"
	pageEncoding="UTF-8"%>
<%@ page isELIgnored="false"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<html lang="en">

<head>
<meta charset="utf-8">
<title><c:out value="${applicationName}"/> - Manage OAuth Consumers</title>
<link type="text/css"
	href="<%=request.getContextPath()%>/oauth/stylesheets/theme.css" rel="stylesheet"></link>
<link type="text/css"
	href="<%=request.getContextPath()%>/oauth/stylesheets/admin.css" rel="stylesheet"></link>
<script data-dojo-config="async: true" type="text/javascript"
	src="http://ajax.googleapis.com/ajax/libs/dojo/1.7.1/dojo/dojo.js">
</script>
<script type="text/javascript" src="<%=request.getContextPath()%>/oauth/scripts/manage.js"></script>
</head>

<body>
	<h1><c:out value="${applicationName}"/> - Manage OAuth Consumers</h1>

	<div class="content">

		<%-- Creative Commons Image: http://openclipart.org/detail/211/shiny-key-by-tiothy --%>
		<img src="<%=request.getContextPath()%>/oauth/images/key.png"
			style="float: right;">

		<div id="error" class="error" style="display: none;"></div>

		<h2>Pending</h2>

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

		<h2>Active</h2>

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
