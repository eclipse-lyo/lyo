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
package org.eclipse.lyo.oslc4j.provider.json4j;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.ext.MessageBodyReader;
import jakarta.ws.rs.ext.MessageBodyWriter;
import jakarta.ws.rs.ext.Provider;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

/**
 * Use JSON-LD support in Jena provider.
 */
@Deprecated
@Provider
@Produces(OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON)
@Consumes(OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON)
public final class OslcCompactJsonProvider extends AbstractOslcRdfJsonProvider
        implements MessageBodyReader<Compact>, MessageBodyWriter<Compact> {
    public OslcCompactJsonProvider() {
        super();
    }

    @Override
    public long getSize(
            final Compact compact,
            final Class<?> type,
            final Type genericType,
            final Annotation[] annotation,
            final MediaType mediaType) {
        return -1;
    }

    @Override
    public boolean isWriteable(
            final Class<?> type,
            final Type genericType,
            final Annotation[] annotations,
            final MediaType mediaType) {
        return (Compact.class.isAssignableFrom(type))
                && (isWriteable(
                        type,
                        annotations,
                        OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON_TYPE,
                        mediaType));
    }

    @Override
    public void writeTo(
            final Compact compact,
            final Class<?> type,
            final Type genericType,
            final Annotation[] annotations,
            final MediaType mediaType,
            final MultivaluedMap<String, Object> map,
            final OutputStream outputStream)
            throws IOException, WebApplicationException {
        writeTo(
                false,
                new Compact[] {compact},
                OslcMediaType.APPLICATION_JSON_TYPE,
                map,
                outputStream);
    }

    @Override
    public boolean isReadable(
            final Class<?> type,
            final Type genericType,
            final Annotation[] annotations,
            final MediaType mediaType) {
        return (Compact.class.isAssignableFrom(type))
                && (isReadable(
                        type, OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON_TYPE, mediaType));
    }

    @Override
    public Compact readFrom(
            final Class<Compact> type,
            final Type genericType,
            final Annotation[] annotations,
            final MediaType mediaType,
            final MultivaluedMap<String, String> map,
            final InputStream inputStream)
            throws IOException, WebApplicationException {
        final Object[] objects =
                readFrom(type, OslcMediaType.APPLICATION_JSON_TYPE, map, inputStream);

        if ((objects != null) && (objects.length > 0)) {
            final Object object = objects[0];

            if (object instanceof Compact) {
                return (Compact) object;
            }
        }

        return null;
    }
}
