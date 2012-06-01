<%@ page import="org.eclipse.lyo.samples.sharepoint.core.IConstants" %>
<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.SharepointInitializer" %>
<%
String uriBase = SharepointInitializer.getBaseUri();
String collection = (String)request.getAttribute("collection");
%>
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
 *.
 -->
<html>
<head>
<title>OSLC Sharepoint Resource Creator</title>
<link rel="SHORTCUT ICON" href="../oslc.png">
<script>
function create(uri){    
	divId = id;
	timeout = setTimeout(function () {
	var req = new XMLHttpRequest();  
	req.open('POST', uri, true);  
	req.setRequestHeader('Accept', 'application/atom+xml');
	req.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
	
	req.onreadystatechange = function (aEvt) {  
		if (req.readyState == 4) {  
			if(req.status == 200) {  
				//debugger;
				//window.confirm(req.responseText);
				var xmlDoc = req.responseXML;
				msftdPrefix = getPrefix(xmlDoc, 'http://schemas.microsoft.com/ado/2007/08/dataservices');
				msftmPrefix = getPrefix(xmlDoc, 'http://schemas.microsoft.com/ado/2007/08/dataservices/metadata');
				var id = xmlDoc.documentElement.getElementsByTagName(msftdPrefix + ':Id')[0];
				var type = xmlDoc.documentElement.getElementsByTagName(msftdPrefix + ':ContentType')[0];
				var name = xmlDoc.documentElement.getElementsByTagName(msftdPrefix + ':Name')[0];
				var msg = document.getElementById("message").innerHTML	= "A " + type + " called " + name + " was created with an Id =" + id;  		
			}   
		}  
	};  
	req.send("title=" + title.value); 
	}, 1000);
}

/* function init() {
	document.getElementById("createResourceForm").onsubmit=function() {
		document.getElementById("createResourceForm").target = "upload_target";
		document.getElementById("upload_target").onload = uploadDone; //This function should be called when the iframe has compleated loading
			// That will happen when the file is completely uploaded and the server has returned the data we need.
	}
}
 */
 
function setTarget(){
  // document.getElementById("createResourceForm").target = "upload_target";
   document.getElementById("upload_target").onload = function() {

	//var ret = frames['upload_target'].document.getElementsByTagName("body")[0].innerHTML;
//	var data = eval("("+ret+")"); //Parse JSON // Read the below explanations before passing judgment on me
	
	var ret = getElementById('upload_target').document.getElementsByTagName("entry")[0];
	
//	if(data.success) { //This part happens when the image gets uploaded.
		document.getElementById("image_details").innerHTML = ret;
//	}
//	else if(data.failure) { //Upload failed - show user the reason.
//		alert("Upload Failed: " + data.failure);
//	}	
  }
}
//window.onload=init;

</script>
</head>
<body>
<form name="createResourceForm" target="upload_target" onsubmit="setTarget()" enctype="multipart/form-data" action="<%=uriBase%>/creator?collection=<%=collection%>" method="post">
Title: (required)<br/>
<input name="title" type="text" style="width:400px" id="title" /><br/>
Description: (optional):<br/>
<textarea name="description" rows="3" cols="100" style="width:400px" id="description"></textarea>
<br/>
File Resource: <input type="file" name="Browse"/><br/>
<button type="submit">Create</button>
<button type="button" onclick="window.close()">Cancel</button>

</form>
<iframe id="upload_target" name="upload_target"  src=""></iframe>
<div id="image_details"></div>

</body>
</html>

