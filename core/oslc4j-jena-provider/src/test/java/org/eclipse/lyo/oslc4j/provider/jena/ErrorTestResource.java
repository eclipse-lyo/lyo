/*
 * Copyright (c) 2025 Contributors to the Eclipse Foundation
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

import java.net.URI;
import javax.xml.namespace.QName;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.ExtendedError;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Response;

/**
 * Test JAX-RS resource for Error responses used in Grizzly-based tests.
 * 
 * @since 7.0.0
 */
@Path("/error-test")
public class ErrorTestResource {

    private static final String DCTERMS_NAMESPACE = "http://purl.org/dc/terms/";

    @GET
    @Path("basic")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_JSON_LD})
    public Response getBasicError() {
        Error error = new Error();
        error.setAbout(URI.create("http://example.com/errors/basic-error"));
        error.setMessage("A basic error message");
        error.setStatusCode("500");
        return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
                .entity(error)
                .build();
    }

    @GET
    @Path("with-extended")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_JSON_LD})
    public Response getErrorWithExtendedError() {
        Error error = new Error();
        error.setAbout(URI.create("http://example.com/errors/extended-error"));
        error.setMessage("Error with extended information");
        error.setStatusCode("400");
        
        ExtendedError extendedError = new ExtendedError();
        extendedError.setMoreInfo(URI.create("http://example.com/help/error-400"));
        extendedError.setHintHeight("300px");
        extendedError.setHintWidth("500px");
        extendedError.setRel("alternate");
        error.setExtendedError(extendedError);
        
        return Response.status(Response.Status.BAD_REQUEST)
                .entity(error)
                .build();
    }

    @GET
    @Path("with-description")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_JSON_LD})
    public Response getErrorWithDescription() {
        Error error = new Error();
        error.setAbout(URI.create("http://example.com/errors/described-error"));
        error.setMessage("Error with dcterms:description");
        error.setStatusCode("500");
        
        // Add dcterms:description as an extended property
        QName descriptionQName = new QName(DCTERMS_NAMESPACE, "description", "dcterms");
        error.getExtendedProperties().put(descriptionQName, 
                "Detailed stack trace: java.lang.NullPointerException at line 42");
        
        return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
                .entity(error)
                .build();
    }

    @GET
    @Path("with-identifier")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_JSON_LD})
    public Response getErrorWithIdentifier() {
        Error error = new Error();
        error.setAbout(URI.create("http://example.com/errors/identified-error"));
        error.setMessage("Resource not found");
        error.setStatusCode("404");
        
        // Add dcterms:identifier as an extended property
        QName identifierQName = new QName(DCTERMS_NAMESPACE, "identifier", "dcterms");
        error.getExtendedProperties().put(identifierQName, "ERR-404-RESOURCE");
        
        return Response.status(Response.Status.NOT_FOUND)
                .entity(error)
                .build();
    }

    @GET
    @Path("with-all-extensions")
    @Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.TEXT_TURTLE, OslcMediaType.APPLICATION_JSON_LD})
    public Response getErrorWithAllExtensions() {
        Error error = new Error();
        error.setAbout(URI.create("http://example.com/errors/full-error"));
        error.setMessage("Comprehensive error with all extensions");
        error.setStatusCode("500");
        
        // Add extended error
        ExtendedError extendedError = new ExtendedError();
        extendedError.setMoreInfo(URI.create("http://example.com/help/500"));
        extendedError.setRel("alternate");
        error.setExtendedError(extendedError);
        
        // Add extended properties
        QName descriptionQName = new QName(DCTERMS_NAMESPACE, "description", "dcterms");
        QName identifierQName = new QName(DCTERMS_NAMESPACE, "identifier", "dcterms");
        
        error.getExtendedProperties().put(descriptionQName, "Full stack trace information");
        error.getExtendedProperties().put(identifierQName, "ERR-500-FULL");
        
        // Add custom type
        error.addType(URI.create("http://example.com/ns#InternalServerError"));
        
        return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
                .entity(error)
                .build();
    }
}
