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
<%@ page contentType="text/html" language="java" %>
<%@ page import="java.util.List" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.Library" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.URLStrategy"%>
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.SharepointInitializer"%>
<%
String collection = (String)request.getAttribute("collection");
%>
<html>
	<head>
		<title>OSLC Sharepoint Adapter: Service Provider for SharePoint Collection \'<%=collection%>\'</title>
<%-- 		<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="stylesheet" type="text/css">
		<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="stylesheet" type="text/css">
		<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="alternate stylesheet" title="Classic" type="text/css">
		<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="alternate stylesheet" title="Classic" type="text/css">
		<link href="<%= bugzillaUri %>/skins/contrib/Dusk/global.css" rel="stylesheet" title="Dusk" type="text/css">
		<link href="<%= bugzillaUri %>/skins/contrib/Dusk/index.css" rel="stylesheet" title="Dusk" type="text/css">
		<link href="<%= bugzillaUri %>/skins/custom/global.css" rel="stylesheet" type="text/css">
		<link href="<%= bugzillaUri %>/skins/custom/index.css" rel="stylesheet" type="text/css">
		<link rel="shortcut icon" href="<%= bugzillaUri %>/images/favicon.ico"> --%>
		<script language="JavaScript">
		
function dialog(url,width,height) {
     var ret = window.showModalDialog(url + "#oslc-core-windowName-1.0",
           "","dialogWidth:" + width + "px; dialogHeight:" + height + "px; center:yes, resizable: yes, status: no, help: no");
}
        </script>
	</head>
	<body onload="">
	
		<div id="header">
			<div id="banner"></div>
			<table border="0" cellspacing="0" cellpadding="0" id="titles">
				<tr>
					<td id="title">
						<p>
							OSLC Sharepoint Adapter: Service Provider
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
			
				<img src="../oslc.png" alt="icon" width="80" height="80" />
	
				<h1>Service Provider for SharePoint collection '<%=collection%>'</h1>
				
				<p>Enables navigation to OSLC Resource Creator and Selector Dialogs</p>

	            <table>
		            <tr>
			            <td><b>This document</b>:</td>
			            <td><a href="<%= URLStrategy.getServiceProviderURL(collection) %>">
			            <%= URLStrategy.getServiceProviderURL(collection) %></a></td>
		            </tr>
		            <tr>
			            <td><b>OSLC Sharepoint</b>:</td>
			            <td><a href="<%= SharepointInitializer.getSharepointUri() %>"><%= SharepointInitializer.getSharepointUri() %></a></td>
		            </tr>
		            <tr>
			            <td><b>Adapter Publisher</b>:</td>
			            <td>OSLC Tools Project</td>
		            </tr>
		            <tr>
			            <td><b>Adapter Identity</b>:</td>
			            <td>org.eclipse.lyo.samples.sharepoint.adapter</td>
		            </tr>
	            </table>
            								
				<h2>OSLC Resource Selector Dialog</h2>
				<p><a href="javascript: dialog('<%= URLStrategy.getDelegatedSelectionURL(collection) %>',350,300)">
				            <%= URLStrategy.getDelegatedSelectionURL(collection) %></a></p>
				
				<h2>OSLC Resource Creator Dialog</h2>
				<p><a href="javascript: dialog('<%= URLStrategy.getDelegatedCreationURL(collection) %>',350,300)">
				            <%= URLStrategy.getDelegatedCreationURL(collection) %></a></p>
				
				<h2>OSLC Resource Listing</h2>            
				<p><a href="list/resource?collection=<%=collection%>">Listing of OSLC Sharepoint Resources for this collection</a> (listing of all Sharepoint Documents)</p>            
			
<%-- 				<h2>OSLC Resource Creation Factory and Resource Shape</h2>
				<p><a href="<%= URLStrategy.getChangeRequestCollectionURL(collection) %>">
				<%= URLStrategy.getChangeRequestCollectionURL(collection) %></a></p>
				<p><a href="<%= URLStrategy.getCreationShapeURL(collection) %>">
				            <%= URLStrategy.getCreationShapeURL(collection) %></a></p>
				
				<h2>OSLC Resource Query Capability and Resource Shape</h2>
				<p><a href="<%= URLStrategy.getChangeRequestCollectionURL(collection) %>">
				<%= URLStrategy.getChangeRequestCollectionURL(collection) %></a></p>
				<p><a href="<%= URLStrategy.getQueryShapeURL(collection) %>">
				            <%= URLStrategy.getQueryShapeURL(collection) %></a></p>
			 --%>
			</div>
		</div>
		
		<div id="footer">
			<div class="intro"></div>
			<div class="outro">
				<div style="margin: 0 1em 1em 1em; line-height: 1.6em; text-align: left">
					<a href="http://open-services.net">OSLC Community</a><br>
				</div>
			</div>
		</div>
	</body>
</html>