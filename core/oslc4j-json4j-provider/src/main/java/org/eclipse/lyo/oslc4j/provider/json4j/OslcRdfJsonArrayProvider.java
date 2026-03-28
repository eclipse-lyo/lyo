/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.oslc4j.provider.json4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.ext.MessageBodyReader;
import jakarta.ws.rs.ext.MessageBodyWriter;
import jakarta.ws.rs.ext.Provider;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

@Provider
@Produces(OslcMediaType.APPLICATION_JSON)
@Consumes(OslcMediaType.APPLICATION_JSON)
public class OslcRdfJsonArrayProvider extends AbstractOslcRdfJsonProvider implements MessageBodyReader<Object[]>, MessageBodyWriter<Object[]> {
    public OslcRdfJsonArrayProvider() {
        super();
    }

    @Override
    public long getSize(final Object[] objects, final Class<?> type, final Type genericType, final Annotation[] annotation,
            final MediaType mediaType) {
        return -1;
    }

    @Override
    public boolean isWriteable(final Class<?> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType) {
        return (type.isArray()) && (isWriteable(type.getComponentType(), annotations, OslcMediaType.APPLICATION_JSON_TYPE, mediaType));
    }

    @Override
    public void writeTo(final Object[] objects, final Class<?> type, final Type genericType, final Annotation[] annotations,
            final MediaType mediaType, final MultivaluedMap<String, Object> map, final OutputStream outputStream)
            throws IOException, WebApplicationException {
        writeTo(true, objects, mediaType, map, outputStream);
    }

    @Override
    public boolean isReadable(final Class<?> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType) {
        return (type.isArray()) && (isReadable(type.getComponentType(), OslcMediaType.APPLICATION_JSON_TYPE, mediaType));
    }

    @Override
    public Object[] readFrom(final Class<Object[]> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType,
            final MultivaluedMap<String, String> map, final InputStream inputStream) throws IOException, WebApplicationException {
        return readFrom(type.getComponentType(), mediaType, map, inputStream);
    }
}