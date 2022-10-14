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

import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * @author Russell Boykin, Alberto Giammaria, Chris Peters, Gianluca Bernardini
 */
@Provider
@Produces(OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML)
@Consumes(OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML)
public class OslcCompactRdfProvider
	   extends AbstractOslcRdfXmlProvider
	   implements MessageBodyReader<Compact>,
				  MessageBodyWriter<Compact>
{
	public OslcCompactRdfProvider()
	{
		super();
	}

	@Override
	public boolean isWriteable(final Class<?> type, final Type genericType,
			final Annotation[] annotations, final MediaType mediaType) {
		return ProviderHelper.isCompactResource(type);
	}

	@Override
	public boolean isReadable(final Class<?> type, final Type genericType,
			final Annotation[] annotations, final MediaType mediaType) {
		return ProviderHelper.isCompactResource(type);
	}

	@Override
	public void writeTo(final Compact						 compact,
						final Class<?>						 type,
						final Type							 genericType,
						final Annotation[]					 annotations,
						final MediaType						 mediaType,
						final MultivaluedMap<String, Object> map,
						final OutputStream					 outputStream)
		   throws IOException,
				  WebApplicationException
	{
		writeTo(false,
				new Compact[] {compact},
				OslcMediaType.APPLICATION_XML_TYPE,
				map,
				outputStream);
	}



	@Override
	public Compact readFrom(final Class<Compact>				 type,
							final Type							 genericType,
							final Annotation[]					 annotations,
							final MediaType						 mediaType,
							final MultivaluedMap<String, String> map,
							final InputStream					 inputStream)
		   throws IOException,
				  WebApplicationException
	{
		final Object[] objects = readFrom(type,
										  OslcMediaType.APPLICATION_XML_TYPE,
										  map,
										  inputStream);

		if ((objects != null) &&
			(objects.length > 0))
		{
			final Object object = objects[0];

			if (object instanceof Compact)
			{
				return (Compact) object;
			}
		}

		return null;
	}

	@Override
	public long getSize(final Compact compact, final Class<?> type, final Type genericType,
			final Annotation[] annotation, final MediaType mediaType) {
		return ProviderHelper.CANNOT_BE_DETERMINED_IN_ADVANCE;
	}
}
