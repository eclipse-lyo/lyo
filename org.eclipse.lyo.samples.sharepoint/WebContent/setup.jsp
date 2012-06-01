<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<%@ page import="org.eclipse.lyo.samples.sharepoint.core.IConstants" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.store.ShareStore" %>
<%
String host = (String) request.getAttribute("host");
String repoPath = (String) request.getAttribute("repoPath");
String binPath = (String) request.getAttribute("binPath");
%>
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
<title>Simple Sharepoint/OSLC Implementation Setup</title>
</head>
<body>
<table><tr><td><a href="/OSLCSharepoint"><img border="0" src="WEB-INF/oslcLg.png"/></a></td><td><h1>Implementation of OSLC<br/> for Microsoft Sharepoint<br/>Setup</h1></td></tr></table>

<p>If this is the first time you've startup this server, you need to configure the server before you can 
start using it.  Once these options have been set this page will not be displayed again.  However you can 
always explicitly re-run the setup anytime in the future by navigating to this URL: 
<b>http://{configured host name}/setup</b>.</p><p>Getting the right host name is critical. Resources are identified by URIs and it is expected and required that these URIs are referenceable by clients.  If a host name has been pre-filled, it was discovered by the servlet container, however you can override this value if you need to.</p>
<p>The RDF Store Path and Binary Resource Store Path must point to a folder on the server's filesystem, and the process that started this service must have full read and create rights to its contents.  This is where the service provider's resource data (in the form of RDF), and all binary resources are stored..</p>
<form method=post action="setup">
<input type="hidden" name="context" value="OSLCSharepoint"/>
Host name:<br/> <input type="text" size="20" name="host" value="<%=host%>"/> &nbsp; (i.e. http://example.com:8080 )<br/>
RDF Store Path: <br/> <input type="text" name="repoPath" value="<%=repoPath%>" size=80/> (i.e. c:\store\repo )<br/>
Binary Resource Store Path: <br/> <input type="text" name="binPath" value="<%=binPath%>" size=80/> (i.e. c:\store\bin )<br/>
<input type="submit" name="Configure" value="Configure"/>

</form>

</body>
</html>