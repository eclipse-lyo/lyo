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

import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_N3;
import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_N_TRIPLES;
import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_RDF_XML;
import static org.eclipse.lyo.core.utils.marshallers.MarshallerConstants.MT_TURTLE;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;

import javax.ws.rs.core.MediaType;
import javax.xml.datatype.DatatypeConfigurationException;

import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFReader;

public class OSLC4JUnmarshaller {
	
	MediaType mediaType = MT_RDF_XML;
	
	OSLC4JUnmarshaller(){}
	
	@SuppressWarnings("unchecked")
	public <T> T unmarshal(InputStream inputStream, Class<T> clazz) throws IllegalArgumentException, SecurityException, DatatypeConfigurationException, IllegalAccessException, InstantiationException, InvocationTargetException, OslcCoreApplicationException, URISyntaxException, NoSuchMethodException{
		if (mediaType == MT_RDF_XML
				|| mediaType == MediaType.APPLICATION_XML_TYPE
				|| mediaType == MT_N_TRIPLES
				|| mediaType == MT_TURTLE
				|| mediaType == MT_N3) {
	        final Model model = ModelFactory.createDefaultModel();
	
	        final RDFReader reader = model.getReader(); // Default reader handles both xml and abbreviated xml
	        
	    	// Pass the empty string as the base URI. This allows Jena to
	    	// resolve relative URIs commonly used to in reified statements
	    	// for OSLC link labels. See this section of the CM specification
	    	// for an example:
	    	// http://open-services.net/bin/view/Main/CmSpecificationV2?sortcol=table;up=#Labels_for_Relationships
	    	reader.read(model,
	    				inputStream,
	    				"");
	
	        Object[] result = JenaModelHelper.fromJenaModel(model, clazz);
	        
	        T ret = null;
	        
	        if(result != null && result.length > 0){
	        	ret = (T)result[0];
	        }
	        
	        return ret;
		}
		
		return null;
        
	}
	
	/**
	 * @return the mediaType
	 */
	public MediaType getMediaType() {
		return mediaType;
	}

	/**
	 * @param mediaType
	 *            the mediaType to set
	 */
	public void setMediaType(MediaType mediaType) {
		this.mediaType = mediaType;
	}
}
