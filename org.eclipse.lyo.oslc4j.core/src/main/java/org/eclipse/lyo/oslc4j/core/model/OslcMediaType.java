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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import javax.ws.rs.core.MediaType;

public interface OslcMediaType {

	public final static String APPLICATION = "application";
	public final static String TEXT = "text";

	public final static String RDF_XML = "rdf+xml";
	public final static String APPLICATION_RDF_XML = APPLICATION + "/" + RDF_XML;
	public final static MediaType APPLICATION_RDF_XML_TYPE = new MediaType(APPLICATION, RDF_XML);

	public final static String JSON_LD = "ld+json";
	public final static String APPLICATION_JSON_LD = APPLICATION + "/" + JSON_LD;
	public final static MediaType APPLICATION_JSON_LD_TYPE = new MediaType(APPLICATION, JSON_LD);

	public final static String APPLICATION_JSON = MediaType.APPLICATION_JSON;
	public final static MediaType APPLICATION_JSON_TYPE = MediaType.APPLICATION_JSON_TYPE;

	public final static String APPLICATION_XML = MediaType.APPLICATION_XML;
	public final static MediaType APPLICATION_XML_TYPE = MediaType.APPLICATION_XML_TYPE;

	public final static String TEXT_XML = MediaType.TEXT_XML;
	public final static MediaType TEXT_XML_TYPE = MediaType.TEXT_XML_TYPE;

	public final static String TURTLE="turtle";
	public final static String TEXT_TURTLE = TEXT + "/" + TURTLE;
	public final static MediaType TEXT_TURTLE_TYPE = new MediaType(TEXT, TURTLE);

	public final static String X_OSLC_COMPACT_XML = "x-oslc-compact+xml";
	public final static String APPLICATION_X_OSLC_COMPACT_XML = APPLICATION + "/" + X_OSLC_COMPACT_XML;
	public final static MediaType APPLICATION_X_OSLC_COMPACT_XML_TYPE = new MediaType(APPLICATION, X_OSLC_COMPACT_XML);

	public final static String X_OSLC_COMPACT_JSON = "x-oslc-compact+json"; // TODO - Compact media type never defined in the OSLC spec for JSON
	public final static String APPLICATION_X_OSLC_COMPACT_JSON = APPLICATION + "/" + X_OSLC_COMPACT_JSON;
	public final static MediaType APPLICATION_X_OSLC_COMPACT_JSON_TYPE = new MediaType(APPLICATION, X_OSLC_COMPACT_JSON);
}
