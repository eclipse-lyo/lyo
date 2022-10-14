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
package org.eclipse.lyo.core.utils.marshallers;

import java.io.OutputStream;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;

import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.oslc4j.core.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.core.json4j.JsonHelper;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.RDFWriterI;
import org.apache.jena.util.FileUtils;

import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.*;

public class OSLC4JMarshaller {
	OSLC4JContext context = new OSLC4JContext();
	MediaType mediaType = MT_RDF_XML;

	OSLC4JMarshaller() {
	}

	public void marshal(Object[] resources, OutputStream os) throws WebApplicationException{

		try {
			if (mediaType.isCompatible(MT_RDF_XML)
					|| mediaType.isCompatible(MediaType.APPLICATION_XML_TYPE)
					|| mediaType.isCompatible(MT_N_TRIPLES)
					|| mediaType.isCompatible(MT_TURTLE)
					|| mediaType.isCompatible(MT_N3)
					|| mediaType.isCompatible(MT_OSLC_COMPACT)) {
				Model model = JenaModelHelper.createJenaModel(resources);
				if (model != null) {
					String format = FileUtils.langXML;
					// XML is the abbreviated format
					if (mediaType.isCompatible(MediaType.APPLICATION_XML_TYPE)
							|| mediaType.isCompatible(MT_OSLC_COMPACT)) {
						format = FileUtils.langXMLAbbrev;
					} else if (mediaType.isCompatible(MT_N_TRIPLES)) {
						format = FileUtils.langNTriple;
					} else if (mediaType.isCompatible(MT_TURTLE)) {
						format = FileUtils.langTurtle;
					}else if (mediaType.isCompatible(MT_N3)) {
						format = FileUtils.langN3;
					}
					final RDFWriterI writer = model.getWriter(format);
					if (mediaType.isCompatible(MT_RDF_XML)
							|| mediaType.isCompatible(MediaType.APPLICATION_XML_TYPE)
							|| mediaType.isCompatible(MT_OSLC_COMPACT)){
						writer.setProperty("showXmlDeclaration", "true");
					}
					writer.write(model, os, null);

				}
			}
			else if(mediaType.isCompatible(MediaType.APPLICATION_JSON_TYPE)){
				JSONObject jo = JsonHelper.createJSON(null, null, null, resources, null);
				jo.write(os);
			}
			else{
				throw new RuntimeException("Unknown Media Type: "  + mediaType);
			}
		} catch (Exception e) {
			throw new WebApplicationException(e);
		}
	}

	/**
	 * @return the mediaType
	 */
	public MediaType getMediaType() {
		return mediaType;
	}

	/**
	 * @param mediaType
	 *			  the mediaType to set
	 */
	public void setMediaType(MediaType mediaType) {
		this.mediaType = mediaType;
	}
}
