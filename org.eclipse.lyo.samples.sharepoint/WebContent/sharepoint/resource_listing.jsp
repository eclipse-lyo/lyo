<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<%@ page import="java.util.*,org.eclipse.lyo.samples.sharepoint.store.*" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.core.IConstants" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.store.ShareStore" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.SharepointInitializer" %>
<%
/* ShareStore store = ShareStore.getStore(); */
String returnUri = SharepointInitializer.getBaseUri() +
                   "/provider?collection=" + 
                  (String)request.getAttribute("collection");

List<Map<String, ShareValue>> results = (List<Map<String, ShareValue>>) request.getAttribute("results");
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
<title>OSLC Sharepoint Resource Listing</title>
<link rel="SHORTCUT ICON" href="../oslc.png">
<script>
var oslcPrefix;
var rdfPrefix;
var divId;
var timeout;
function getPrefix(xmlDoc,ns) {
	var attrs = xmlDoc.documentElement.attributes;
	for( var i=0; i<attrs.length; i++ ) {
		if( attrs[i].nodeValue == ns ) {
			var name = attrs[i].nodeName;
			var pos = name.indexOf(":");
			return ( name.substring(pos+1) );
		}
	}
}
function hover(uri,id){    
	divId = id;
	timeout = setTimeout(function () {
	var req = new XMLHttpRequest();  
	req.open('GET', uri, true);  
	req.setRequestHeader('Accept', 'application/x-oslc-compact+xml');
	req.onreadystatechange = function (aEvt) {  
		if (req.readyState == 4) {  
			if(req.status == 200) {  
				//debugger;
				var xmlDoc = req.responseXML;
				oslcPrefix = getPrefix(xmlDoc, 'http://open-services.net/ns/core#');
				rdfPrefix = getPrefix(xmlDoc, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
				var smPreview = xmlDoc.documentElement.getElementsByTagName(oslcPrefix + ':smallPreview')[0];
				if( smPreview ) {
					var oslcDoc = smPreview.getElementsByTagName(oslcPrefix + ':document')[0];
					if( oslcDoc ) {
						var url = oslcDoc.getAttribute(rdfPrefix + ':resource');
						if( oslcDoc ) {
							var div = document.getElementById(divId);
							if( div ) {
								var elmHintWidth = smPreview.getElementsByTagName(oslcPrefix + ':hintWidth')[0];
								var divWidth = elmHintWidth.textContent;
								var elmHintHeight = smPreview.getElementsByTagName(oslcPrefix + ':hintHeight')[0];
								var divHeight = elmHintHeight.textContent;
								div.innerHTML = '<'+'object type="text/html" data="'+url+'" width="'+divWidth+'" height="'+divHeight+'" style="background-color:#ffffee; border-style:solid;border-width:2px;"><\/object>';
							}
						}
					}
				}
			}   
		}  
	};  
	req.send(null); 
	}, 1000);
}
function closeHover() { 
	if( divId ) {
	    if(timeout) {clearTimeout(timeout);} 
		var elmDiv = document.getElementById(divId);
		if( elmDiv ) {
			elmDiv.innerHTML = '';
			elmDiv.width = null;
			elmDiv.height = null;
		}
	}
}
</script>
</head>
<body>
<p><a href="<%=returnUri%>">Back</a></p>
<h3>OSLC Sharepoint Resource Listing</h3>
<table>
<%
Iterator i = results.iterator();
while( i.hasNext() ) {
	// List<Map<String, RioValue>>
	Map<String, ShareValue> row = (Map<String, ShareValue>) i.next(); 
	String title = row.get("title").stringValue();
	String uri = row.get("uri").stringValue(); 
	int pos = uri.lastIndexOf('/');
	String id = uri.substring(pos+1);
%>
<tr><td>
<a href="<%=uri%>" onmouseover="hover('<%=uri %>','d<%=id %>');" onmouseout="closeHover();"><%=uri %></a> <b><%=title %> (<%=id %>)</b>
<div id="d<%=id %>"></div>
</td></tr>

<%}%>
</table>
</body>
</html>