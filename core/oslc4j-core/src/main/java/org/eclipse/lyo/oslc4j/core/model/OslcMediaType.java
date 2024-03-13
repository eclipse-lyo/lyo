/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.oslc4j.core.model;

import jakarta.ws.rs.core.MediaType;

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
