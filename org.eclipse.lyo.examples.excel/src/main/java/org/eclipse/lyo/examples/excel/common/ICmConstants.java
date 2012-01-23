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
package org.eclipse.lyo.examples.excel.common;

import org.eclipse.lyo.rio.core.IConstants;

@SuppressWarnings("nls")
public interface ICmConstants extends IConstants {
	
	// server constants
	public static final String SERVER_CONTEXT = "sori-cm";  
	public static final int DEFAULT_MAX_RESULTS = 100;
	
	public static final String RIO_CM_PUBLISHER_TITLE = "Open Services for Lifecycle Collaboration in Change Management"; 
	public static final String RIO_CM_PUBLISHER_IDENTIFIER = "open-services.net/sori/cm"; 
	public static final String RIO_CM_ICON = "oslc.png"; 
	
	// service segments
	public static final String SERVICE_PROVIDER_CATALOG = "catalog";
	public static final String SERVICE_CHANGEREQUEST = "changerequest";		  
	public static final String SERVICE_FACTORY_CHANGEREQUEST = "factory/changerequest";  
	public static final String SERVICE_SELECTOR_CHANGEREQUEST = "selector/changerequest";  
	public static final String SERVICE_CREATOR_CHANGEREQUEST = "creator/changerequest";  
	
	//RIO
	public static final String RIO_CM_NAMESPACE = "http://open-services.net/rio/cm/"; 
	public static final String RIO_CM_PREFIX = "rio_cm"; 
	public static final String RIO_CM_XMLS_DECL = "\txmlns:" + RIO_CM_PREFIX + "=\"" +  RIO_CM_NAMESPACE + "\"\n";  

	public static final String RIO_CM_SELECTION_RESOURCE_WIDTH = "300px";
	public static final String RIO_CM_SELECTION_RESOURCE_HEIGHT = "300px";
	public static final String RIO_CM_CREATION_RESOURCE_WIDTH = "300px";
	public static final String RIO_CM_CREATION_RESOURCE_HEIGHT = "250px";

	// OSLC CM
	public static final String OSLC_CM_NAMESPACE = "http://open-services.net/ns/cm#";
	public static final String OSLC_CM_PREFIX = "oslc_cm";
	
	public static final String OSLC_CM_TERM_CHANGEREQUEST = "ChangeRequest";  
	public static final String OSLC_CM_CHANGEREQUEST = OSLC_CM_NAMESPACE + OSLC_CM_TERM_CHANGEREQUEST;  
	public static final String OSLC_CM_PTERM_CHANGEREQUEST = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_CHANGEREQUEST;  
	
	// OSLC CM Change Requirements Properties
	public static final String OSLC_CM_STATUS_SUBMITTED = "Submitted";
	public static final String OSLC_CM_STATUS_IN_PROGRESS = "In Progress";
	public static final String OSLC_CM_STATUS_DONE = "Done";
	public static final String OSLC_CM_STATUS_FIXED = "Fixed";
	
	public static final String[] OSLC_CM_STATUS_VALUES = { 
		OSLC_CM_STATUS_SUBMITTED, 
		OSLC_CM_STATUS_IN_PROGRESS, 
		OSLC_CM_STATUS_DONE, 
		OSLC_CM_STATUS_FIXED 
	};

	
	public static final String OSLC_CM_TERM_STATUS = "status";  
	public static final String OSLC_CM_PTERM_STATUS = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_STATUS ;  
	public static final String OSLC_CM_STATUS = OSLC_CM_NAMESPACE + OSLC_CM_TERM_STATUS;  
	
	public static final String OSLC_CM_TERM_CLOSED = "closed";  
	public static final String OSLC_CM_PTERM_CLOSED = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_CLOSED ;  
	public static final String OSLC_CM_CLOSED = OSLC_CM_NAMESPACE + OSLC_CM_TERM_CLOSED;  
	
	public static final String OSLC_CM_TERM_INPROGRESS = "inprogress";  
	public static final String OSLC_CM_PTERM_INPROGRESS = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_INPROGRESS ;  
	public static final String OSLC_CM_INPROGRESS = OSLC_CM_NAMESPACE + OSLC_CM_TERM_INPROGRESS;  
	
	public static final String OSLC_CM_TERM_FIXED = "fixed";  
	public static final String OSLC_CM_PTERM_FIXED = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_FIXED ;  
	public static final String OSLC_CM_FIXED = OSLC_CM_NAMESPACE + OSLC_CM_TERM_FIXED;  
	
	public static final String OSLC_CM_TERM_APPROVED = "approved";  
	public static final String OSLC_CM_PTERM_APPROVED = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_APPROVED;  
	public static final String OSLC_CM_APPROVED = OSLC_CM_NAMESPACE + OSLC_CM_TERM_APPROVED;  
	
	public static final String OSLC_CM_TERM_REVIEWED = "reviewed";  
	public static final String OSLC_CM_PTERM_REVIEWED = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_REVIEWED;  
	public static final String OSLC_CM_REVIEWED = OSLC_CM_NAMESPACE + OSLC_CM_TERM_REVIEWED;  
	
	public static final String OSLC_CM_TERM_VERIFIED = "verified";  
	public static final String OSLC_CM_PTERM_VERIFIED = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_VERIFIED ;  
	public static final String OSLC_CM_VERIFIED = OSLC_CM_NAMESPACE + OSLC_CM_TERM_VERIFIED;  
	
	public static final String OSLC_CM_TERM_RELATEDCHANGEREQUEST = "relatedChangeRequest";  
	public static final String OSLC_CM_PTERM_RELATEDCHANGEREQUEST = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_RELATEDCHANGEREQUEST;  
	public static final String OSLC_CM_RELATEDCHANGEREQUEST = OSLC_CM_NAMESPACE + OSLC_CM_TERM_RELATEDCHANGEREQUEST;  
	
	public static final String OSLC_CM_TERM_AFFECTSPLANITEM = "affectsPlanItem";  
	public static final String OSLC_CM_PTERM_AFFECTSPLANITEM = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_AFFECTSPLANITEM;  
	public static final String OSLC_CM_AFFECTSPLANITEM = OSLC_CM_NAMESPACE + OSLC_CM_TERM_AFFECTSPLANITEM;  
	
	public static final String OSLC_CM_TERM_AFFECTEDBYDEFECT = "affectdByDefect";  
	public static final String OSLC_CM_PTERM_AFFECTEDBYDEFECT = OSLC_CM_PREFIX + ':' + OSLC_CM_TERM_AFFECTEDBYDEFECT;  
	public static final String OSLC_CM_AFFECTEDBYDEFECT = OSLC_CM_NAMESPACE + OSLC_CM_TERM_AFFECTEDBYDEFECT;  
	
}
