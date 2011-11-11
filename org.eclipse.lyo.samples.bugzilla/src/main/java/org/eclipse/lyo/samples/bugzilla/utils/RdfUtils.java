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
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.bugzilla.utils;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

import thewebsemantic.Bean2RDF;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.vocabulary.RDF;

/**
 * Utilities for working with RDF.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class RdfUtils {
	private static final Map<String, String> PREFIXES = new HashMap<String, String>();
	static {
		PREFIXES.put("oslc", "http://open-services.net/ns/core#");
		PREFIXES.put("oslc_cm", "http://open-services.net/ns/cm#");
		PREFIXES.put("dcterms", "http://purl.org/dc/terms/");
		PREFIXES.put("foaf", "http://xmlns.com/foaf/0.1/");
		PREFIXES.put("bugz", "http://www.bugzilla.org/rdf#");
	}
	
	public static final String JENA_LANG_RDF_XML = "RDF/XML";
	public static final String JENA_LANG_TURTLE = "TURTLE";
	public static final String JENA_LANG_ABBREVIATED_RDF_XML = "RDF/XML-ABBREV";

	public static Model createModel() {
		Model m = ModelFactory.createDefaultModel();
		m.setNsPrefixes(PREFIXES);
		return m;
	}

	public static void writeModel(HttpServletResponse response, Model model, String lang)
			throws IOException {
		removeUnnecessaryStatements(model);
		model.write(response.getOutputStream(), lang);
		response.flushBuffer();
	}

	public static void sendErrorResponse(HttpServletResponse response,
			org.eclipse.lyo.samples.bugzilla.resources.Error error, String lang)
			throws IOException {
		response.setStatus(error.getStatusCode());
		sendRdfResponse(response, error, lang);
	}

	public static void sendRdfResponse(HttpServletResponse response,
			Object resource, String lang) throws IOException {
		Model m = createModel();
		Bean2RDF writer = new Bean2RDF(m);
		writer.save(resource);
		writeModel(response, m, lang);
	}
	
	/**
	 * Remove some extra stuff Jenabean puts in the model that we don't want.
	 * 
	 * @param model
	 *            the model
	 */
	public static void removeUnnecessaryStatements(Model model) {
		model.removeAll(
				model.createResource("http://thewebsemantic.com/javaclass"),
				null, null);
		model.removeAll(null,
				model.createProperty("http://thewebsemantic.com/javaclass"),
				null);
		model.removeAll(null, RDF.type, model
				.createResource("http://www.w3.org/2000/01/rdf-schema#Class"));
	}
}
