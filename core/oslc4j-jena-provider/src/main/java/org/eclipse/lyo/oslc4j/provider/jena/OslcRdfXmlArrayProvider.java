/*
 * Copyright (c) 2012-2019 IBM Corporation and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */
package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyReader;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * @author Russell Boykin, Alberto Giammaria, Chris Peters, Gianluca Bernardini, Andrew Berezovskyi
 */
@Provider
@Produces({OslcMediaType.APPLICATION_RDF_XML})
@Consumes({OslcMediaType.APPLICATION_RDF_XML})
public class OslcRdfXmlArrayProvider
	   extends AbstractOslcRdfXmlProvider
	   implements MessageBodyReader<Object[]>,
				  MessageBodyWriter<Object[]>
{
	public OslcRdfXmlArrayProvider()
	{
		super();
	}

	@Override
	public boolean isWriteable(final Class<?> type, final Type genericType,
			final Annotation[] annotations, final MediaType mediaType) {
		return true;
	}

	@Override
	public boolean isReadable(final Class<?> type, final Type genericType,
			final Annotation[] annotations, final MediaType mediaType) {
		return true;
	}

	@Override
	public void writeTo(final Object[]						 objects,
						final Class<?>						 type,
						final Type							 genericType,
						final Annotation[]					 annotations,
						final MediaType						 mediaType,
						final MultivaluedMap<String, Object> map,
						final OutputStream					 outputStream)
		   throws IOException,
				  WebApplicationException {

		writeTo(ProviderHelper.isQueryResult(type, annotations), objects, mediaType, map,
				outputStream);
	}

	@Override
	public Object[] readFrom(final Class<Object[]>				  type,
							 final Type							  genericType,
							 final Annotation[]					  annotations,
							 final MediaType					  mediaType,
							 final MultivaluedMap<String, String> map,
							 final InputStream					  inputStream)
		   throws IOException,
				  WebApplicationException
	{
		return readFrom(type.getComponentType(),
						mediaType,
						map,
						inputStream);
	}

	@Override
	public long getSize(final Object[] objects, final Class<?> type, final Type genericType,
			final Annotation[] annotations, final MediaType mediaType) {
		return ProviderHelper.CANNOT_BE_DETERMINED_IN_ADVANCE;
	}
}
