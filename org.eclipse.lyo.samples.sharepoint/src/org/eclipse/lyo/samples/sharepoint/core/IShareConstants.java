/*******************************************************************************
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
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.core;

@SuppressWarnings("nls")
public interface IShareConstants {

	public static final String SERVER_SCHEME = "http://";  
	public static final int DEFAULT_MAX_RESULTS = 100;
	
	// Share
	public static final String SHARE_PUBLISHER_TITLE = "Open Services for Lifecycle Collaboration in Architecture Management"; 
	public static final String SHARE_PUBLISHER_IDENTIFIER = "open-services.net/ri/am"; 
	public static final String SHARE_ICON = "oslc.png"; 
	
	public static final String SHARE_URI = "http://open-services.net/ri"; 
	public static final String SHARE_NAMESPACE = "http://open-services.net/ri/"; 
	public static final String SHARE_PREFIX = "oslc"; 
	public static final String SHARE_XMLS_DECL = "\txmlns:" + SHARE_PREFIX + "=\"" +  SHARE_NAMESPACE + "\"\n";  

	public static final String URI_COUNTER = SHARE_NAMESPACE + "uriCounter"; 
	public static final String URI_SERVER = SHARE_NAMESPACE + "server"; 

	public static final String SHARE_UNKNOWN_USER_ID = "_UNKNOWN_USER_";


	// additional content types
	public static final String CT_APP_X_VND_MSPPT = "application/vnd.ms-powerpoint"; 

}
