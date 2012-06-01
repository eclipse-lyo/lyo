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
package org.eclipse.lyo.samples.sharepoint.common;

import org.eclipse.lyo.samples.sharepoint.core.IConstants;

@SuppressWarnings("nls")
public interface IAmConstants extends IConstants {
	
	// server constants
	public static final String SERVER_CONTEXT = "";//"rio-am";  

	//
	public static final String RIO_AM_PUBLISHER_TITLE = "Open Services for Lifecycle Collaboration in Architecture Management"; 
	public static final String RIO_AM_PUBLISHER_IDENTIFIER = "open-services.net/rio/am"; 
	public static final String RIO_AM_ICON = "oslc.png"; 
	
	// service segments
	public static final String SERVICE_RESOURCE = "resource";  
	public static final String SERVICE_SOURCE = "source";  
	public static final String SERVICE_LINKTYPE = "linktype";  
	public static final String SERVICE_FACTORY_RESOURCE = "factory/resource";  
	public static final String SERVICE_FACTORY_SOURCE = "factory/source";  
	public static final String SERVICE_FACTORY_LINKTYPE = "factory/linktype";  
	public static final String SERVICE_SELECTOR_RESOURCE = "selector/resource";  
	public static final String SERVICE_SELECTOR_LINKTYPE = "selector/linktype";  
	public static final String SERVICE_CREATOR_RESOURCE = "creator/resource";  
	public static final String SERVICE_CREATOR_LINKTYPE = "creator/linktype";  
	
	// RIO

	public static final String RIO_AM_URI = "http://open-services.net/ri/am"; 
	public static final String RIO_AM_NAMESPACE = "http://open-services.net/ri/am/"; 
	public static final String RIO_AM_PREFIX = "rio"; 
	public static final String RIO_AM_XMLS_DECL = "\txmlns:" + RIO_AM_PREFIX + "=\"" +  RIO_AM_NAMESPACE + "\"\n";  
	public static final String RIO_AM_SOURCE_CONTENT_TYPE = RIO_AM_NAMESPACE + "sourceContentType"; 

	public static final String RIO_AM_TERM_SEARCHTERMS = RIO_AM_NAMESPACE + "searchTerms";  
	public static final String RIO_AM_PTERM_SEARCHTERMS = RIO_AM_PREFIX + ":searchTerms";  
	public static final String RIO_AM_SEARCHTERMS = RIO_AM_NAMESPACE + "searchTerms";  
	public static final String RIO_AM_UNKNOWN_USER_ID = "_UNKNOWN_USER_";
	
	public static final String RIO_AM_SELECTION_RESOURCE_WIDTH = "320px";
	public static final String RIO_AM_SELECTION_RESOURCE_HEIGHT = "250px";
	public static final String RIO_AM_SELECTION_LINKTYPE_WIDTH = "320px";
	public static final String RIO_AM_SELECTION_LINKTYPE_HEIGHT = "250px";
	public static final String RIO_AM_CREATION_RESOURCE_WIDTH = "320px";
	public static final String RIO_AM_CREATION_RESOURCE_HEIGHT = "250px";
	public static final String RIO_AM_CREATION_LINKTYPE_WIDTH = "320px";
	public static final String RIO_AM_CREATION_LINKTYPE_HEIGHT = "250px";

	public static final String RIO_AM_PPT_DECK = "http://open-services.net/ri/am/ppt/deck";
	public static final String RIO_AM_PPT_SLIDE = "http://open-services.net/ri/am/ppt/slide";

	// oslc_am
	public static final String OSLC_AM_NAMESPACE = "http://open-services.net/ns/am#";
	public static final String OSLC_AM_PREFIX = "oslc_am";
	public static final String OSLC_AM_XMLS_DECL = "\txmlns:" + OSLC_AM_PREFIX + "=\"" + OSLC_AM_NAMESPACE + "\"\n";

	// oslc_am types 
	public static final String OSLC_AM_TYPE_TERM_RESOURCE = "Resource";
	public static final String OSLC_AM_TYPE_PTERM_RESOURCE = OSLC_AM_PREFIX + ':' + OSLC_AM_TYPE_TERM_RESOURCE;
	public static final String OSLC_AM_TYPE_RESOURCE = OSLC_AM_NAMESPACE + OSLC_AM_TYPE_TERM_RESOURCE;
	public static final String OSLC_AM_TYPE_TERM_LINKTYPE = "LinkType";
	public static final String OSLC_AM_TYPE_PTERM_LINKTYPE = OSLC_AM_PREFIX + ':' + OSLC_AM_TYPE_TERM_LINKTYPE;
	public static final String OSLC_AM_TYPE_LINKTYPE = OSLC_AM_NAMESPACE + OSLC_AM_TYPE_TERM_LINKTYPE;

	
	// additional content types
	public static final String CT_APP_X_VND_MSPPT = "application/vnd.ms-powerpoint"; 

}
