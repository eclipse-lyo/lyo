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
