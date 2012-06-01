<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<%@ page import="org.eclipse.lyo.samples.sharepoint.core.IConstants" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.store.*"%>
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.SharepointInitializer"%>
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.SharepointResource,java.util.*,org.eclipse.lyo.samples.sharepoint.util.*" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.common.IAmConstants" %>
<%
System.out.println("entered resource_view.jsp");
///ShareStore store = ShareStore.getStore();
String uriBase = SharepointInitializer.getBaseUri();
SharepointResource resource = (SharepointResource) request.getAttribute("resource"); 
String uri = resource.getUri();
String title = resource.getTitle();
String description = resource.getDescription();
String created = resource.getCreated().toString();
String modified = resource.getModified().toString();
String approvalstatus = resource.getApprovalStatus();
String eTag = "etag should be set"; //resource.getETag();
XmlFormatter formatter = new XmlFormatter();
formatter.addNamespacePrefix(IAmConstants.OSLC_AM_NAMESPACE,IAmConstants.OSLC_AM_PREFIX); 
//String rawRdf = formatter.format(resource, IAmConstants.OSLC_AM_TYPE_RESOURCE);
//String rdfXml = XmlUtils.encode(rawRdf);
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
<title>OSLC Sharepoint Resource View (<%=uri%>)</title>
<link rel="SHORTCUT ICON" href="../oslc.png">
</head>
<body>
<p><a href="<%=uriBase%>/index.jsp">Home</a></p>
<h1>OSLC Sharepoint Resource: <%=title %></h1>
<form action="<%=uriBase%>/web/resource" method="POST">
<input type="hidden" name="uri" value="<%=uri %>"/>
<input type="hidden" name="eTag" value="<%=eTag %>"/>
<input type="hidden" name="title" value="<%=title %>"/>
<table>
<tr><td>URI:</td><td><b><%=uri %></b></td></tr>
<tr><td>Title:</td><td><b><%=title %></b></td></tr>
<tr><td>Created:</td><td><b><%=created %></b></td></tr>
<tr><td>Modified:</td><td><b><%=modified %></b></td></tr>
<tr><td>Approval Status:</td><td><b><%=approvalstatus %></b></td></tr>
<tr><td>ETag:</td><td><b><%=eTag %></b></td></tr>
</table>
<input type="submit" name="Open Document" value="Open"/>
<!-- <input type="submit" name="Save" value="Save"/> -->
</form>
<%-- <h3>RDF/XML</h3>
<form action="<%=uriBase%>/web/resource" method="POST">
<input type="hidden" name="uri" value="<%=uri %>"/>
<input type="hidden" name="eTag" value="<%=eTag %>"/>
<pre><textarea name="rdfxml" rows=20 cols=132 id="rdfxml"><%=rdfXml %></textarea></pre>
<input type="submit" name="Save" value="SaveRdf"/>
</form> --%>
</body>
</html>