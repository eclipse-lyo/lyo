<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<%--
 Copyright (c) 2011 IBM Corporation.

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 
 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.
 
 Contributors:
 
    IBM Corporation - initial API and implementation
--%>
<%@page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%@page import="org.eclipse.lyo.samples.bugzilla.jbugzx.base.Product"%>
<%@ page contentType="text/html" language="java" %>
<%@ page import="com.j2bugzilla.base.*" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.jbugzx.base.*" %>
<%@ page import="java.util.List" %>
<%
	List<Bug> bugs = (List<Bug>)request.getAttribute("results"); 
	String bugzillaUri = (String) request.getAttribute("bugzillaUri");
	String queryUri = (String)request.getAttribute("queryUri");
	String nextPageUri = (String)request.getAttribute("nextPageUri");
	Product product = (Product)request.getAttribute("product");
%>
<html>
	<head>
		<title>Bugzilla OSLC Adapter: Service Provider for <%= product.getName() + "(" + product.getId() + ")" %></title>
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
							Bugzilla OSLC Adapter: Service Provider
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
	
				<h1>Query Results</h1>

                <% for (Bug bug : bugs) { %>                
                <p>Summary: <%= bug.getSummary() %><br /><a href="<%= URLStrategy.getChangeRequestURL(bug.getID()) %>">
                	<%= URLStrategy.getChangeRequestURL(bug.getID()) %></a></p>
			    <% } %>
            	<% if (nextPageUri != null) { %><a href="<%= nextPageUri %>">Next Page</a><% } %>

			</div>
		</div>
		
		<div id="footer">
			<div class="intro"></div>
			<div class="outro">
				<div style="margin: 0 1em 1em 1em; line-height: 1.6em; text-align: left">
					<b>OSLC Tools Adapter Server 0.1</b> brought to you by the <a href="http://open-services.net">OSLC Community</a><br />
				</div>
			</div>
		</div>
	</body>
</html>

