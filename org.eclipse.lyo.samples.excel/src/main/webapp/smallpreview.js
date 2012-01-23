/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     Masaki Wakao 
 *     Yoshio Horiuchi 
 *     Kohji Ohsawa 
 *******************************************************************************/
var oslcPrefix;
var rdfPrefix;
var divId;
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
	var req = new XMLHttpRequest();  
	req.open('GET', uri, true);  
	req.setRequestHeader('Accept', 'application/x-oslc-compact+xml');
	req.onreadystatechange = function (aEvt) {  
		//if (req.readyState == 4) {  
//			if(req.status == 200) {  
//				debugger;
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
//				}
			//}   
		}  
	};  
	req.send(null); 
}
function closeHover() { 
	if( divId ) {
		var elmDiv = document.getElementById(divId);
		if( elmDiv ) {
			elmDiv.innerHTML = '';
			elmDiv.width = null;
			elmDiv.height = null;
		}
	}
}