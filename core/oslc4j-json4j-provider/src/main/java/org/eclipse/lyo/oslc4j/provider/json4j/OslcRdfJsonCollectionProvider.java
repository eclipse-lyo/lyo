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
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.ext.MessageBodyReader;
import jakarta.ws.rs.ext.MessageBodyWriter;
import jakarta.ws.rs.ext.Provider;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

import org.eclipse.lyo.oslc4j.provider.json4j.internal.RdfCollections;

@Provider
@Produces(OslcMediaType.APPLICATION_JSON)
@Consumes(OslcMediaType.APPLICATION_JSON)
public class OslcRdfJsonCollectionProvider extends AbstractOslcRdfJsonProvider
        implements MessageBodyReader<Collection<Object>>, MessageBodyWriter<Collection<Object>> {
    public OslcRdfJsonCollectionProvider() {
        super();
    }

    @Override
    public long getSize(final Collection<Object> collection, final Class<?> type, final Type genericType, final Annotation[] annotation,
            final MediaType mediaType) {
        return -1;
    }

    @Override
    public boolean isWriteable(final Class<?> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType) {
        if ((Collection.class.isAssignableFrom(type)) && (genericType instanceof ParameterizedType)) {
            final ParameterizedType parameterizedType = (ParameterizedType) genericType;

            final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

            if (actualTypeArguments.length == 1) {
                final Type actualTypeArgument = actualTypeArguments[0];

                if (actualTypeArgument instanceof Class) {
                    return isWriteable((Class<?>) actualTypeArgument, annotations, OslcMediaType.APPLICATION_JSON_TYPE, mediaType);
                }
            }
        }

        return false;
    }

    @Override
    public void writeTo(final Collection<Object> collection, final Class<?> type, final Type genericType, final Annotation[] annotations,
            final MediaType mediaType, final MultivaluedMap<String, Object> map, final OutputStream outputStream)
            throws IOException, WebApplicationException {
        writeTo(true, collection.toArray(new Object[collection.size()]), mediaType, map, outputStream);
    }

    @Override
    public boolean isReadable(final Class<?> type, final Type genericType, final Annotation[] annotations, final MediaType mediaType) {
        if ((Collection.class.isAssignableFrom(type)) && (genericType instanceof ParameterizedType)) {
            final ParameterizedType parameterizedType = (ParameterizedType) genericType;

            final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

            if (actualTypeArguments.length == 1) {
                final Type actualTypeArgument = actualTypeArguments[0];

                if (URI.class.equals(actualTypeArgument) && (OslcMediaType.APPLICATION_JSON_TYPE.isCompatible(mediaType))) {
                    return true;
                } else {
                    return isReadable((Class<?>) actualTypeArgument, OslcMediaType.APPLICATION_JSON_TYPE, mediaType);
                }
            }
        }

        return false;
    }

    @Override
    @SuppressWarnings("java:S3776") // Complex legacy method
    public Collection<Object> readFrom(final Class<Collection<Object>> type, final Type genericType, final Annotation[] annotations,
            final MediaType mediaType, final MultivaluedMap<String, String> map, final InputStream inputStream)
            throws IOException, WebApplicationException {
        if (genericType instanceof ParameterizedType) {
            final ParameterizedType parameterizedType = (ParameterizedType) genericType;

            final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

            if (actualTypeArguments.length == 1) {
                final Type actualTypeArgument = actualTypeArguments[0];

                if (actualTypeArgument instanceof Class) {
                    final Object[] objects = readFrom((Class<?>) actualTypeArgument, mediaType, map, inputStream);

                    final Collection<Object> collection;
                    try {
                        collection = RdfCollections.createCollection(type);
                    } catch (final Exception exception) {
                        throw new WebApplicationException(exception, buildBadRequestResponse(exception, mediaType, map));
                    }

                    collection.addAll(Arrays.asList(objects));

                    return collection;
                }
            }
        }

        return null; // NOSONAR it is intended to return null
    }
}