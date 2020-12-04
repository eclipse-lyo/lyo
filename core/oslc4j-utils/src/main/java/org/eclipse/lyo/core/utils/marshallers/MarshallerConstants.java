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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.core.utils.marshallers;

import javax.ws.rs.core.MediaType;

public class MarshallerConstants {
	//Media Types
	public static String CT_RDF_XML = "application/rdf+xml";
	public static String CT_N_TRIPLES = "text/nt";
	public static String CT_TURTLE = "text/turtle";
	public static String CT_N3 = "text/n3";
	public static String CT_OSLC_COMPACT = "application/x-oslc-compact+xml";
	
	public static MediaType MT_RDF_XML = MediaType.valueOf(CT_RDF_XML);
	public static MediaType MT_N_TRIPLES = MediaType.valueOf(CT_N_TRIPLES);
	public static MediaType MT_TURTLE= MediaType.valueOf(CT_TURTLE);
	public static MediaType MT_N3 = MediaType.valueOf(CT_N3);
	public static MediaType MT_OSLC_COMPACT = MediaType.valueOf(CT_OSLC_COMPACT);
}
