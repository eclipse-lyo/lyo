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
<%@ page import="com.j2bugzilla.base.*" %>
<%@ page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%
int productId= (Integer)request.getAttribute("productId");
String bugzillaUri = (String) request.getAttribute("bugzillaUri");
%>
<html>
<head>
<title>Bugzilla OSLC Adapter: Resource Selector</title>
<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/global.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%= bugzillaUri %>/skins/standard/index.css" rel="alternate stylesheet" title="Classic" type="text/css">
<link href="<%= bugzillaUri %>/skins/contrib/Dusk/global.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%= bugzillaUri %>/skins/contrib/Dusk/index.css" rel="stylesheet" title="Dusk" type="text/css">
<link href="<%= bugzillaUri %>/skins/custom/global.css" rel="stylesheet" type="text/css">
<link href="<%= bugzillaUri %>/skins/custom/index.css" rel="stylesheet" type="text/css">
<link rel="shortcut icon" href="<%= bugzillaUri %>/images/favicon.ico">

<script type="text/javascript" src="bugzilla.js"></script>

</head>
<body>
	
	<input type="text" style="width: 200px" id="searchTerms" />
	<button type="button" onclick="search( '<%= URLStrategy.getDelegatedSelectionURL(productId) %>' )">Search</button>	
	<br/><b>Change Requests</b><br/>
	
	<table>
		<tr><td><select id="results" size="10" style="width: 300px"></select></td></tr>
		<tr><td><button type="button" onclick="javascript: select();">OK</button>
		<button type="button" onclick="javascript: cancel()">Cancel</button></td></tr>
	</table>
	
</body>
</html>