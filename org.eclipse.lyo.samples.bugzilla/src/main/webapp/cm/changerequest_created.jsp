<!DOCTYPE html>
<html>
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
<%@ page import="java.net.*,java.util.*,java.text.SimpleDateFormat" %> 
<%@ page import="com.j2bugzilla.base.Bug" %>
<% 
Bug    bug     = (Bug)request.getAttribute("bug");
String bugUri  = (String)request.getAttribute("bugUri");
String title   = bug.getSummary();
%>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
<script type="text/javascript">
   // Step #2: read the return URL 
   var returnURL = window.name;

   // Step #4: send the response via the window.name variable 
   var response = "oslc-response:{\"oslc:results\" \: [{ \"oslc:label\" : \"<%= title %>\", \"rdf:resource\" : \"<%= bugUri %>\"}]}";
   window.name = response;

   // Step #5: indicate that user has responded 
   window.location.href = returnURL;
</script>
</head>
<body>
</body>
</html>
