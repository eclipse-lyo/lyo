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

import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.ResponseBuilder;
import jakarta.ws.rs.ext.Providers;

import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.exception.MessageExtractor;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfoArray;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.eclipse.lyo.oslc4j.provider.json4j.internal.LyoProviderUtils;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;
import jakarta.json.JsonWriter;
import jakarta.json.stream.JsonGenerator;

public abstract class AbstractOslcRdfJsonProvider {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractOslcRdfJsonProvider.class);

    private static final Annotation[] ANNOTATIONS_EMPTY_ARRAY = new Annotation[0];
    private static final Class<Error> CLASS_OSLC_ERROR = Error.class;

    private @Context HttpHeaders httpHeaders; // Available only on the server
    protected @Context HttpServletRequest httpServletRequest; // Available only on the server
    private @Context Providers providers; // Available on both client and server

    protected AbstractOslcRdfJsonProvider() {
        super();
    }

    @SuppressWarnings("java:S3776") // Complex legacy method
    protected static boolean isWriteable(final Class<?> type, final Annotation[] annotations, final MediaType requiredMediaType,
            final MediaType actualMediaType) {
        if (type.getAnnotation(OslcResourceShape.class) != null) {
            // When handling "recursive" writing of an OSLC Error object, we get a zero-length array of annotations
            if ((annotations != null) && ((annotations.length > 0) || (Error.class != type))) {
                for (final Annotation annotation : annotations) {
                    if (annotation instanceof Produces) {
                        final Produces producesAnnotation = (Produces) annotation;

                        for (final String value : producesAnnotation.value()) {
                            if (requiredMediaType.isCompatible(MediaType.valueOf(value))) {
                                return true;
                            }
                        }
                    }
                }

                return false;
            }

            // We do not have annotations when running from the non-web client.
            return requiredMediaType.isCompatible(actualMediaType);
        }

        return false;
    }

    protected void writeTo(final boolean queryResult, final Object[] objects, final MediaType errorMediaType,
            final MultivaluedMap<String, Object> map, final OutputStream outputStream) throws WebApplicationException {
        boolean isClientSide = false;

        try {
            httpServletRequest.getMethod();
        } catch (RuntimeException e) {
            isClientSide = true;
        }

        String descriptionURI = null;
        String responseInfoURI = null;

        if (queryResult && !isClientSide) {

            final String method = httpServletRequest.getMethod();
            if ("GET".equals(method)) {
                descriptionURI = LyoProviderUtils.resolveURI(httpServletRequest);
                responseInfoURI = descriptionURI;

                final String queryString = httpServletRequest.getQueryString();
                if ((queryString != null) && (isOslcQuery(queryString))) {
                    responseInfoURI += "?" + queryString;
                }
            }

        }

        final JsonObject jsonObject;

        @SuppressWarnings("unchecked")
        final Map<String, Object> properties = isClientSide ? null
                : (Map<String, Object>) httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_SELECTED_PROPERTIES);
        final String nextPageURI = isClientSide ? null : (String) httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_NEXT_PAGE);
        final Integer totalCount = isClientSide ? null : (Integer) httpServletRequest.getAttribute(OSLC4JConstants.OSLC4J_TOTAL_COUNT);

        ResponseInfo<?> responseInfo = new ResponseInfoArray<>(null, properties, totalCount, nextPageURI);

        try {
            jsonObject = JsonHelper.createJSON(descriptionURI, responseInfoURI, responseInfo, objects, properties);

            createPrettyPrintingWriter(outputStream).writeObject(jsonObject);
        } catch (final Exception exception) { // NOSONAR
            LOGGER.warn(MessageExtractor.getMessage("ErrorSerializingResource"), exception);
            throw new WebApplicationException(exception, buildServerErrorResponse(exception, errorMediaType, map));
        }
    }

    @SuppressWarnings("java:S107") // too many params on legacy method
    protected void writeTo(final Object[] objects, final MediaType errorMediaType, final MultivaluedMap<String, Object> map,
            final OutputStream outputStream, final Map<String, Object> properties, final String descriptionURI, final String responseInfoURI,
            final ResponseInfo<?> responseInfo) throws WebApplicationException {
        final JsonObject jsonObject;

        try {
            jsonObject = JsonHelper.createJSON(descriptionURI, responseInfoURI, responseInfo, objects, properties);

            createPrettyPrintingWriter(outputStream).writeObject(jsonObject);
        } catch (final Exception exception) {// NOSONAR
            LOGGER.warn(MessageExtractor.getMessage("ErrorSerializingResource"), exception);
            throw new WebApplicationException(exception, buildServerErrorResponse(exception, errorMediaType, map));
        }
    }

    private static JsonWriter createPrettyPrintingWriter(OutputStream outputStream) {
        Map<String, Object> properties = Collections.singletonMap(JsonGenerator.PRETTY_PRINTING, true);
        return Json.createWriterFactory(properties).createWriter(outputStream);
    }

    protected static boolean isReadable(final Class<?> type, final MediaType requiredMediaType, final MediaType actualMediaType) {
        return (type.getAnnotation(OslcResourceShape.class) != null) && (requiredMediaType.isCompatible(actualMediaType));
    }

    protected Object[] readFrom(final Class<?> type, final MediaType errorMediaType, final MultivaluedMap<String, String> map,
            final InputStream inputStream) throws WebApplicationException {
        try (JsonReader reader = Json.createReader(inputStream)) {
            final JsonObject jsonObject = reader.readObject();
            return JsonHelper.fromJSON(jsonObject, type);
        } catch (final Exception exception) {
            throw new WebApplicationException(exception, buildBadRequestResponse(exception, errorMediaType, map));
        }
    }

    protected Response buildBadRequestResponse(final Exception exception, final MediaType errorMediaType, final MultivaluedMap<String, ?> map) {
        return buildResponse(exception, errorMediaType, map, Response.Status.BAD_REQUEST);
    }

    protected Response buildServerErrorResponse(final Exception exception, final MediaType errorMediaType, final MultivaluedMap<String, ?> map) {
        return buildResponse(exception, errorMediaType, map, Response.Status.INTERNAL_SERVER_ERROR);
    }

    private Response buildResponse(final Exception exception, final MediaType errorMediaType, final MultivaluedMap<String, ?> map,
            Response.Status status) {
        try {
            final MediaType determinedErrorMediaType = determineErrorMediaType(errorMediaType, map);

            final Error error = new Error();

            error.setStatusCode(String.valueOf(status.getStatusCode()));
            error.setMessage(exception.getMessage());

            final ResponseBuilder responseBuilder = Response.status(status);
            return responseBuilder.type(determinedErrorMediaType).entity(error).build();
        } catch (UndeclaredThrowableException e) {
            // determineErrorMediaType may raise an UndeclaredThrowableException with an infinite stack trace
            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).build();
        }
    }

    /**
     * We handle the case where a client requests an accept type different than their content type.
     * This will work correctly in the successful case. But, in the error case where our
     * MessageBodyReader/MessageBodyWriter fails internally, we respect the client's requested accept type.
     */
    @SuppressWarnings("java:S3776") // Complex legacy method
    private MediaType determineErrorMediaType(final MediaType initialErrorMediaType, final MultivaluedMap<String, ?> map) {
        try {
            // HttpHeaders will not be available on the client side and will throw a NullPointerException
            final List<MediaType> acceptableMediaTypes = httpHeaders.getAcceptableMediaTypes();

            // Since we got here, we know we are executing on the server

            if (acceptableMediaTypes != null) {
                for (final MediaType acceptableMediaType : acceptableMediaTypes) {
                    // If a concrete media type with a MessageBodyWriter
                    if (isAcceptableMediaType(acceptableMediaType)) {
                        return acceptableMediaType;
                    }
                }
            }
        } catch (final NullPointerException exception) {
            // Ignore NullPointerException since this means the context has not been set since we are on the client

            // See if the MultivaluedMap for headers is available
            if (map != null) {
                final Object acceptObject = map.getFirst("Accept");

                if (acceptObject instanceof String) {
                    final String[] accepts = acceptObject.toString().split(",");

                    double winningQ = 0.0D;
                    MediaType winningMediaType = null;

                    for (final String accept : accepts) {
                        final MediaType acceptMediaType = MediaType.valueOf(accept);

                        // If a concrete media type with a MessageBodyWriter
                        if (isAcceptableMediaType(acceptMediaType)) {
                            final String stringQ = acceptMediaType.getParameters().get("q");

                            final double q = stringQ == null ? 1.0D : Double.parseDouble(stringQ);

                            // Make sure and exclude blackballed media type
                            if (Double.compare(q, 0.0D) > 0) {
                                if ((winningMediaType == null) || (Double.compare(q, winningQ) > 0)) { // NOSONAR
                                    winningMediaType = acceptMediaType;
                                    winningQ = q;
                                }
                            }
                        }
                    }

                    if (winningMediaType != null) {
                        return winningMediaType;
                    }
                }
            }
        }

        return initialErrorMediaType;
    }

    /**
     * Only allow media types that are not wildcards and have a related MessageBodyWriter for OSLC Error.
     */
    private boolean isAcceptableMediaType(final MediaType mediaType) {
        return (!mediaType.isWildcardType()) && (!mediaType.isWildcardSubtype())
                && (providers.getMessageBodyWriter(CLASS_OSLC_ERROR, CLASS_OSLC_ERROR, ANNOTATIONS_EMPTY_ARRAY, mediaType) != null);
    }

    protected static boolean isOslcQuery(final String parmString) {
        boolean containsOslcParm = false;

        final String[] uriParts = parmString.toLowerCase(Locale.getDefault()).split("oslc\\.", 2);
        if (uriParts.length > 1) {
            containsOslcParm = true;
        }

        return containsOslcParm;
    }
}