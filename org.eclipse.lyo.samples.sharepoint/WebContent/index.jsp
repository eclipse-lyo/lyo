<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<%@ page import="org.eclipse.lyo.samples.sharepoint.common.IAmConstants" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
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
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<link rel="SHORTCUT ICON" href="WEB-INF/oslc.png">
<title>Simple OSLC Sharepoint Implementation </title>
<script language="JavaScript">
function dialog(url,width,height) {
     var ret = window.showModalDialog(url + "#oslc-core-windowName-1.0",
           "","dialogWidth:" + width + "px; dialogHeight:" + height + "px; center:yes, resizable: yes, status: no, help: no");
}
</script>
<base target="_self" />
</head>
<body>
<table><tr><td><img src="WEB-INF/oslcLg.png"/></td><td><h2>Home:<br/>OSLC Sharepoint Implementation<br/></h2></td></tr></table>
<p><a href="about.jsp">About OSLC Sharepoint</a></p>
<p><b>OSLC Services</b></p>
<blockquote>
<p><a href="catalog">Catalog</a> (OSLC Catalog document)</p>
<p><a href="provider">Service Provider</a> (OSLC ServiceProvider document)</p>
<p><a href="javascript: dialog('creator/resource',450,200)">UI Resource Creator</a> (Delegated UI resource creator)</p>
<p><a href="javascript: dialog('selector/resource',350,300)">UI Resource Picker</a> (Delegated UI resource selector)</p>
</blockquote>
<p><b>Server Admin Tasks and Utilities:</b></p>
<blockquote>
<br/>
<p><a href="list/resource">Listing of OSLC Sharepoint Resources</a> (listing of all Sharepoint Documents)</p>
<!-- <p><a href="sparql">SPARQL</a> (Free form SPARQL of RDF Store)</p> -->
</blockquote>
</body>
</html>