<%@ page import="org.eclipse.lyo.samples.sharepoint.adapter.SharepointInitializer" %>
<%@ page import="java.util.*,org.eclipse.lyo.samples.sharepoint.store.*" %>
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
 *
 -->
<html>
<head>
<script type="text/javascript">


function search(){
	list = document.getElementById("results");
	list.options.length = 0;
	xmlhttp = new XMLHttpRequest();
//	xmlhttp.setRequestHeader("Accept","application/json");
	terms = document.getElementById("searchTerms").value;
	/* todo make query work, for now make not null */
	terms="all";
	xmlhttp.open("GET","<%=uriBase%>/selector?collection=<%=collection%>&terms=" + escape(terms),true);
	xmlhttp.onreadystatechange = function() {
		if (xmlhttp.readyState==4 && xmlhttp.status==200) {
			// populate results
			txt = xmlhttp.responseText;
			resp = eval('(' + txt + ')');
			for( i=0; i<resp.results.length; i=i+1 ) {
				var item=document.createElement('option');
				item.text = resp.results[i].title;
				item.value = resp.results[i].uri;
				list.add(item, null); 
			}
		}
	}
	
	xmlhttp.send();
}

function select(){
	list = document.getElementById("results");
	if( list.length>0 && list.selectedIndex >= 0 ) {
		option = list.options[list.selectedIndex];

		var oslcResponse = '{ "oslc:results": [ ' +  
			' { "oslc:label":"' + option.text + '", "rdf:resource":"' + option.value + '"} ' + 
		' ]}';
		
		 respondWithPostMessage(oslcResponse);
		if (window.location.hash == '#oslc-core-windowName-1.0') {       
      	  // Window Name protocol in use
	        respondWithWindowName(oslcResponse);
		} else if (window.location.hash == '#oslc-core-postMessage-1.0') {
        	// Post Message protocol in use
			respondWithPostMessage(oslcResponse);
		} 
	}
}

function respondWithWindowName(/*string*/ response) {
   // Step #2: read the return URL
   var returnURL = window.name;

   // Step #4: send the response via the window.name variable
   window.name = response;

   // Step #5: indicate that user has responded
   window.location = returnURL;
}

function respondWithPostMessage(/*string*/ response) {
	if( window.parent != null ) {
		window.parent.postMessage(response, "*");
	} else {
		window.postMessage(response, "*");
	}
}

function cancel(){
	window.parent.close();
}
</script>
<link rel="SHORTCUT ICON" href="../oslc.png">
</head>
<body>
<input type="text" style="width: 200px" id="searchTerms" />
<button type="button" onclick="search()">Search</button>
<br/><b>Resources</b><br/>
<table>
<tr><td><select id="results" size="10" style="width: 300px"></select></td></tr>
<tr><td><button type="button" onclick="select()">OK</button>
<button type="button" onclick="cancel()">Cancel</button></td></tr>
</table>
</body>
</html>
