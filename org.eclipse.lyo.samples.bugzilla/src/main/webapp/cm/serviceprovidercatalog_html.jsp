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
<%@ page import="java.util.List" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%
String bugzillaUri = (String) request.getAttribute("bugzillaUri");
List<Product> products = (List<Product>)request.getAttribute("products");
%>
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
		<title>Bugzilla OSLC Adapter: Service Provider Catalog</title>
		<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="stylesheet" type="text/css">
		<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="stylesheet" type="text/css">
		<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="alternate stylesheet" title="Classic" type="text/css">
		<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="alternate stylesheet" title="Classic" type="text/css">
		<link href="<%= bugzillaUri %>/skins/contrib/Dusk/global.css" rel="stylesheet" title="Dusk" type="text/css">
		<link href="<%= bugzillaUri %>/skins/contrib/Dusk/index.css" rel="stylesheet" title="Dusk" type="text/css">
		<link href="<%= bugzillaUri %>/skins/custom/global.css" rel="stylesheet" type="text/css">
		<link href="<%= bugzillaUri %>/skins/custom/index.css" rel="stylesheet" type="text/css">
		<link rel="shortcut icon" href="<%= bugzillaUri %>/images/favicon.ico">
	</head>
	<body onload="">
	
		<div id="header">
			<div id="banner"></div>
			<table border="0" cellspacing="0" cellpadding="0" id="titles">
				<tr>
					<td id="title">
						<p>
							Bugzilla OSLC Adapter: Service Provider Catalog
						</p>
					</td>
					<td id="information">
						<p class="header_addl_info">
							version 0.1
						</p>
					</td>
				</tr>
			</table>
		</div>
		
		<div id="bugzilla-body">
			<div id="page-index">
			
			<img src="bugzilla.gif" alt="icon" width="80" height="80" />
			
			<h1>Service Provider Catalog</h1>
			
			<p>Enables navigation to Service Provider for each Product against which bugs may be reported.</p>

            <table>
	            <tr>
		            <td><b>This document</b>:</td>
		            <td><a href="<%= URLStrategy.getServiceProviderCatalogURL() %>">
                            <%= URLStrategy.getServiceProviderCatalogURL() %>
                        </a></td>
	            </tr>
	            <tr>
		            <td><b>Bugzilla</b>:</td>
		            <td><a href="<%= bugzillaUri %>"><%= bugzillaUri %></a></td>
	            </tr>
	            <tr>
		            <td><b>Adapter Publisher</b>:</td>
		            <td>OSLC Tools Project</td>
	            </tr>
	            <tr>
		            <td><b>Adapter Identity</b>:</td>
		            <td>org.eclipse.lyo.samples.bugzilla.test</td>
	            </tr>
            </table>
			
			<h2>Service Providers</h2>
			
			<% for (Product p : products) { %>
			<h3>Service Provider for Product <%= p.getName() %></h3>
			<p><a href="<%= URLStrategy.getServiceProviderURL(p.getId()) %>">
                    <%= URLStrategy.getServiceProviderURL(p.getId()) %></a></p>
			<% } %>			
			
			</div>
		</div>
		
		<div id="footer">
			<div class="intro"></div>
			<div class="outro">
				<div style="margin: 0 1em 1em 1em; line-height: 1.6em; text-align: left">
					<b>OSLC Tools Adapter Server 0.1</b> brought to you by the <a href="http://open-services.net">OSLC Community</a><br>
				</div>
			</div>
		</div>
	</body>
</html>
