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
 
    IBM Corporation - initial API and implementation
--%>
<%@ page contentType="text/html" language="java" pageEncoding="UTF-8" %>
<%@page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%@ page import="java.util.List" %>
<%@ page import="com.j2bugzilla.base.Product" %>
<%
	String bugzillaUri = (String) request.getAttribute("bugzillaUri");
	Product product = (Product) request.getAttribute("product");
	List<String> operatingSystems = (List<String>) request
			.getAttribute("operatingSystems");
	List<String> components = (List<String>) request
			.getAttribute("components");
	List<String> platforms = (List<String>) request
			.getAttribute("platforms");
	List<String> versions = (List<String>) request
			.getAttribute("versions");
%>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
<title>Bugzilla OSLC Adapter: Resource Creator</title>
<link href="<%=bugzillaUri%>/skins/standard/global.css" rel="stylesheet" type="text/css">
<link href="<%=bugzillaUri%>/skins/standard/index.css" rel="stylesheet" type="text/css">
<link href="<%=bugzillaUri%>/skins/standard/global.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%=bugzillaUri%>/skins/standard/index.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%=bugzillaUri%>/skins/contrib/Dusk/global.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%=bugzillaUri%>/skins/contrib/Dusk/index.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%=bugzillaUri%>/skins/custom/global.css" rel="stylesheet" type="text/css">
<link href="<%=bugzillaUri%>/skins/custom/index.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="<%=bugzillaUri%>/images/favicon.ico">
<script type="text/javascript" src="bugzilla.js"></script>
</head>
<body style="padding: 10px;">
	<div id="bugzilla-body">
    <form id="Create" class="enter_bug_form">
	<input name="product" type="hidden" value="<%=product.getName()%>" />

			<table style="clear: both;">

				<tr>
					<th class="field_label required">Summary:</th>
					<td><input name="summary" class="required text_input"
						type="text" style="width: 400px" id="summary" required autofocus></td>
				</tr>

				<tr>
					<th class="field_label">Component:</th>
					<td><select name="component">
							<%
								for (String c : components) {
							%>
							<option value="<%=c%>"><%=c%></option>
							<%
								}
							%>
					</select></td>
				</tr>

				<tr>
					<th class="field_label">Version:</th>
					<td><select name="version">
							<%
								for (String v : versions) {
							%>
							<option value="<%=v%>"><%=v%></option>
							<%
								}
							%>
					</select></td>
				</tr>

				<tr>
					<th class="field_label">OS:</th>
					<td><select name="op_sys">
							<%
								for (String os : operatingSystems) {
							%>
							<option value="<%=os%>"><%=os%></option>
							<%
								}
							%>
					</select></td>
				</tr>

				<tr>
					<th class="field_label">Platform:</th>
					<td><select name="platform">
							<%
								for (String p : platforms) {
							%>
							<option value="<%=p%>"><%=p%></option>
							<%
								}
							%>
					</select></td>
				</tr>

				<tr>
					<th class="field_label">Description:</th>
					<td><textarea style="width: 400px; height: 150px;"
							id="description" name="description"></textarea></td>
				</tr>
				
				<tr>
					<td></td>
					<td>
						<input type="button"
							value="Submit Bug"
							onclick="javascript: create( '<%=URLStrategy.getDelegatedCreationURL(product.getID())%>' )">
						<input type="button" value="Cancel" onclick="javascript: cancel()">
					</td>
				</tr>
			</table>

			<div sytle="width: 500px;">
				
			</div>
			
		</form>

		<div style="clear: both;"></div>
	</div>
</body>
</html>

