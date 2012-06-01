<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<!--
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 -->
<%@ page contentType="text/html" language="java"%>
<%@ page import="java.util.List" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.Library" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.URLStrategy"%>
<%
String sharepointUri = (String) request.getAttribute("sharepointUri");
List<Library> libraries = (List<Library>)request.getAttribute("libraries");
%>
<html>
	<head>
		<title>OSLC Sharepoint Adapter: Service Provider Catalog</title>
		<link href="<%= sharepointUri %>/skins/standard/global.css" rel="stylesheet" type="text/css">
		<link href="<%= sharepointUri %>/skins/standard/index.css" rel="stylesheet" type="text/css">
		<link href="<%= sharepointUri %>/skins/standard/global.css" rel="alternate stylesheet" title="Classic" type="text/css">
		<link href="<%= sharepointUri %>/skins/standard/index.css" rel="alternate stylesheet" title="Classic" type="text/css">
		<link href="<%= sharepointUri %>/skins/contrib/Dusk/global.css" rel="stylesheet" title="Dusk" type="text/css">
		<link href="<%= sharepointUri %>/skins/contrib/Dusk/index.css" rel="stylesheet" title="Dusk" type="text/css">
		<link href="<%= sharepointUri %>/skins/custom/global.css" rel="stylesheet" type="text/css">
		<link href="<%= sharepointUri %>/skins/custom/index.css" rel="stylesheet" type="text/css">
	</head>
	<body onload="">
	
		<div id="header">
			<div id="banner"></div>
			<table border="0" cellspacing="0" cellpadding="0" id="titles">
				<tr>
					<td id="title">
						<p>
							OSLC Sharepoint Adapter: Service Provider Catalog
						</p>
					</td>
					<td id="information">
						<p class="header_addl_info"> version 0.1</p>
					</td>
				</tr>
			</table>
		</div>
		
		<div id="sharepoint-body">
			<div id="page-index">
				
			
			<h1>Service Provider Catalog</h1>
			
			<p>Enables navigation to a Service Provider for each Sharepoint Library.</p>

            <table>
	            <tr>
		            <td><b>This document</b>:</td>
		            <td><a href="<%= URLStrategy.getServiceProviderCatalogURL() %>">
                            <%= URLStrategy.getServiceProviderCatalogURL() %>
                        </a></td>
	            </tr>
	            <tr>
		            <td><b>Sharepoint</b>:</td>
		            <td><a href="<%= sharepointUri %>"><%= sharepointUri %></a></td>
	            </tr>
	            <tr>
		            <td><b>Adapter Publisher</b>:</td>
		            <td>IBM Software Standards</td>
	            </tr>
	            <tr>
		            <td><b>Adapter Identity</b>:</td>
		            <td>org.eclipse.lyo.samples.sharepoint.adapter</td>
	            </tr>
            </table>
			
			<h2>Service Providers</h2>
			
			<% for (Library l : libraries) { %>
			<h3>Service Provider for Sharepoint Library <%= l.getName() %></h3>
			<p><a type="text/html" href="<%= l.getUri() %>">
                    <%= l.getUri() %></a></p>
			<% } %>			
			
			</div>
		</div>
		
		<div id="footer">
			<div class="intro"></div>
			<div class="outro">
				<div style="margin: 0 1em 1em 1em; line-height: 1.6em; text-align: left">
					<b>OSLC Sharepoint Adapter</b><br>
				</div>
			</div>
		</div>
	</body>
</html>
