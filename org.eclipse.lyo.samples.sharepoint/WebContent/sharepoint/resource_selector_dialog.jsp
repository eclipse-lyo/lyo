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
<script language="JavaScript">
function windowNameProtocol(/*string*/ dialogURI, onDataReceived) {

	   // Step #1: create iframe for Delegated UI Dialog, 
	   // adding fragment to indicate the protocol
	   var frame = doc.createElement('iframe');
	   frame.src= url + '#oslc-core-postMessage-1.0';

	   // Step #2: set the iframe's window.name to indicate the Return URL
	   frame.name = "http://localhost/blank.html";

	   // Step #3: listen for onload events on the iframe
	   frame.onload = function() {

	      try { // May throw an exception if the frame's location is still a different origin

	         // Step #4: when frame's location is equal to the Return URL 
	         // then read response and return.
	         if (frame.contentWindow.location == returnLocation) {
	            var message = frame.contentWindow.name;
	            destroyFrame(frame);
	            handleMessage(message);
	         }
	      } catch(e) {
	         // ignore: access exception when trying to access window name
	      };
	   };
	   displayFrame(frame);
}

</script>
<title>Insert title here</title>
</head>
<body>
</body>
</html>