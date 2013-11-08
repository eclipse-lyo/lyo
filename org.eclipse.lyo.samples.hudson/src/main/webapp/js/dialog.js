/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *     Samuel Padgett - initial implementation
 *******************************************************************************/

function respondWithWindowName(/*string*/ response) {
   var returnURL = window.name;
   window.name = response;
   window.location.href = returnURL;
}

function respondWithPostMessage(/*string*/ response) {
	if( window.parent != null ) {
		window.parent.postMessage(response, "*");
	} else {
		window.postMessage(response, "*");
	}
}

function respond(/*string*/ response) {
	response = 'oslc-response:' + response;

	if (window.location.hash == '#oslc-core-windowName-1.0') {       
		// Window Name protocol in use
		respondWithWindowName(response);
	} else if (window.location.hash == '#oslc-core-postMessage-1.0') {
		// Post Message protocol in use
		respondWithPostMessage(response);
	} 
}