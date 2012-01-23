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
package org.eclipse.lyo.samples.excel.changerequest;

import java.io.IOException;
import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyReader;
import javax.ws.rs.ext.Provider;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

@Provider
//@Consumes ({"application/rdf+xml", "application/xml"})
public class ChangeRequestReader implements MessageBodyReader<Model> {

	@Override
	public boolean isReadable(Class<?> type, Type genericType, Annotation[] annotations,
			MediaType arg3) {
		return true;
	}

	@Override
	public Model readFrom(Class type, Type genericType, Annotation[] annotations,
			MediaType contentType, MultivaluedMap httpHeaders, InputStream content)
			throws IOException, WebApplicationException {
		
		try{
			Model model = ModelFactory.createDefaultModel();
			model.read(content, "");
			model.write(System.out,"RDF/XML-ABBREV");
			return model;
		}catch(Exception e){
			e.printStackTrace();
		}	
		return null;
	}
	
}

