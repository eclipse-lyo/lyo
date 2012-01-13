<html>
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
<%@ page import="java.net.*,java.util.*,java.text.SimpleDateFormat" %> 
<%@ page import="com.j2bugzilla.base.Bug" %>
<% 
Bug    bug     = (Bug)request.getAttribute("bug");
String bugUri  = (String)request.getAttribute("bugUri");
String title   = bug.getSummary();
%>
<head>
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
