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
<%@ page contentType="application/json" language="java" pageEncoding="UTF-8" %>
<%@page import="org.eclipse.lyo.samples.bugzilla.URLStrategy"%>
<%@ page import="java.net.*,java.util.*" %> 
<%@ page import="com.j2bugzilla.base.Bug" %>
{
<% 
List<Bug> results = (List<Bug>)request.getAttribute("results");
%>
results: [
<% int i = 0; for (Bug b : results) { %>
   <% if (i > 0) { %>,<% } %>
   {  "title" : "<%= URLStrategy.getChangeRequestLinkLabel(b.getID(), b.getSummary()) %>",
      "resource" : "<%= URLStrategy.getChangeRequestURL(b.getID()) %>"
   }
<% i++; } %>
]
}
