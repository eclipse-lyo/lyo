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
 *    Kevin Bauer - Initial implementation
 *******************************************************************************/
package org.eclipse.lyo.core.utils.marshallers;

import javax.ws.rs.core.MediaType;

public class MarshallerConstants {
	//Media Types
	public static String CT_RDF_XML = "application/rdf+xml";
	public static String CT_N_TRIPLES = "text/nt";
	public static String CT_TURTLE = "text/turtle";
	public static String CT_N3 = "text/n3";
	
	public static MediaType MT_RDF_XML = MediaType.valueOf(CT_RDF_XML);
	public static MediaType MT_N_TRIPLES = MediaType.valueOf(CT_N_TRIPLES);
	public static MediaType MT_TURTLE= MediaType.valueOf(CT_TURTLE);
	public static MediaType MT_N3 = MediaType.valueOf(CT_N3);
}
