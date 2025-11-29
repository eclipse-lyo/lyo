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
package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.namespace.QName;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.ExtendedError;
import org.eclipse.lyo.oslc4j.core.model.IExtendedResource;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Tests for the OSLC Error class with JenaModelHelper.
 * These tests verify that Error properly supports extended properties 
 * as required by OSLC Core specification.
 * 
 * @see <a href="https://github.com/oslc-op/oslc-specs/issues/466#issuecomment-762351427">Issue reference</a>
 * @since 7.0.0
 */
class ErrorJenaModelHelperTest {

    private static final Logger log = LoggerFactory.getLogger(ErrorJenaModelHelperTest.class);
    private static final String DCTERMS_NAMESPACE = "http://purl.org/dc/terms/";

    @Test
    @DisplayName("Error class should implement IExtendedResource interface")
    void errorShouldImplementIExtendedResource() {
        Error error = new Error();
        assertThat(error).isInstanceOf(IExtendedResource.class);
    }

    @Test
    @DisplayName("Error marshalling and unmarshalling should preserve basic properties")
    void errorMarshalUnmarshalShouldPreserveBasicProperties() throws Exception {
        // Given an Error with basic properties
        Error error = new Error();
        error.setMessage("Test error message");
        error.setStatusCode("500");

        // When serializing and deserializing
        Model model = JenaModelHelper.createJenaModel(new Object[]{error});
        logModel(model);

        Error[] errors = JenaModelHelper.unmarshal(model, Error.class);

        // Then basic properties should be preserved
        assertThat(errors).hasSize(1);
        assertThat(errors[0].getMessage()).isEqualTo("Test error message");
        assertThat(errors[0].getStatusCode()).isEqualTo("500");
    }

    @Test
    @DisplayName("Error should support extended error information")
    void errorShouldSupportExtendedError() throws Exception {
        // Given an Error with extended error
        Error error = new Error();
        error.setMessage("Test error with extended info");
        error.setStatusCode("400");
        
        ExtendedError extendedError = new ExtendedError();
        extendedError.setMoreInfo(URI.create("http://example.com/moreinfo"));
        extendedError.setHintHeight("200px");
        extendedError.setHintWidth("400px");
        extendedError.setRel("alternate");
        error.setExtendedError(extendedError);

        // When serializing and deserializing
        Model model = JenaModelHelper.createJenaModel(new Object[]{error});
        logModel(model);

        Error[] errors = JenaModelHelper.unmarshal(model, Error.class);

        // Then extended error should be preserved
        assertThat(errors).hasSize(1);
        assertThat(errors[0].getExtendedError()).isNotNull();
        assertThat(errors[0].getExtendedError().getMoreInfo())
                .isEqualTo(URI.create("http://example.com/moreinfo"));
        assertThat(errors[0].getExtendedError().getHintHeight()).isEqualTo("200px");
        assertThat(errors[0].getExtendedError().getHintWidth()).isEqualTo("400px");
        assertThat(errors[0].getExtendedError().getRel()).isEqualTo("alternate");
    }

    @Test
    @DisplayName("Error should support dcterms:description as extended property")
    void errorShouldSupportDctermsDescription() throws Exception {
        // Given an Error with dcterms:description
        Error error = new Error();
        error.setMessage("Error occurred");
        error.setStatusCode("500");
        
        // Add dcterms:description as an extended property
        QName descriptionQName = new QName(DCTERMS_NAMESPACE, "description", "dcterms");
        error.getExtendedProperties().put(descriptionQName, "Detailed stack trace or additional information");

        // When serializing and deserializing
        Model model = JenaModelHelper.createJenaModel(new Object[]{error});
        logModel(model);

        Error[] errors = JenaModelHelper.unmarshal(model, Error.class);

        // Then dcterms:description should be preserved
        assertThat(errors).hasSize(1);
        assertThat(errors[0].getExtendedProperties()).containsKey(descriptionQName);
        assertThat(errors[0].getExtendedProperties().get(descriptionQName))
                .isEqualTo("Detailed stack trace or additional information");
    }

    @Test
    @DisplayName("Error should support dcterms:identifier as extended property")
    void errorShouldSupportDctermsIdentifier() throws Exception {
        // Given an Error with dcterms:identifier
        Error error = new Error();
        error.setMessage("Resource not found");
        error.setStatusCode("404");
        
        // Add dcterms:identifier as an extended property
        QName identifierQName = new QName(DCTERMS_NAMESPACE, "identifier", "dcterms");
        error.getExtendedProperties().put(identifierQName, "ERR-404-001");

        // When serializing and deserializing
        Model model = JenaModelHelper.createJenaModel(new Object[]{error});
        logModel(model);

        Error[] errors = JenaModelHelper.unmarshal(model, Error.class);

        // Then dcterms:identifier should be preserved
        assertThat(errors).hasSize(1);
        assertThat(errors[0].getExtendedProperties()).containsKey(identifierQName);
        assertThat(errors[0].getExtendedProperties().get(identifierQName)).isEqualTo("ERR-404-001");
    }

    @Test
    @DisplayName("Error should support multiple extended properties")
    void errorShouldSupportMultipleExtendedProperties() throws Exception {
        // Given an Error with multiple extended properties
        Error error = new Error();
        error.setMessage("Complex error");
        error.setStatusCode("500");
        
        QName descriptionQName = new QName(DCTERMS_NAMESPACE, "description", "dcterms");
        QName identifierQName = new QName(DCTERMS_NAMESPACE, "identifier", "dcterms");
        
        error.getExtendedProperties().put(descriptionQName, "Stack trace details");
        error.getExtendedProperties().put(identifierQName, "ERR-500-XYZ");

        // When serializing and deserializing
        Model model = JenaModelHelper.createJenaModel(new Object[]{error});
        logModel(model);

        Error[] errors = JenaModelHelper.unmarshal(model, Error.class);

        // Then all extended properties should be preserved
        assertThat(errors).hasSize(1);
        assertThat(errors[0].getExtendedProperties()).hasSize(2);
        assertThat(errors[0].getExtendedProperties().get(descriptionQName))
                .isEqualTo("Stack trace details");
        assertThat(errors[0].getExtendedProperties().get(identifierQName))
                .isEqualTo("ERR-500-XYZ");
    }

    @Test
    @DisplayName("Error should support RDF types")
    void errorShouldSupportRdfTypes() throws Exception {
        // Given an Error with additional RDF type
        Error error = new Error();
        error.setMessage("Custom error");
        error.setStatusCode("400");
        error.addType(URI.create("http://example.com/ns#CustomError"));

        // When serializing and deserializing
        Model model = JenaModelHelper.createJenaModel(new Object[]{error});
        logModel(model);

        Error[] errors = JenaModelHelper.unmarshal(model, Error.class);

        // Then the additional type should be preserved
        assertThat(errors).hasSize(1);
        assertThat(errors[0].getTypes()).contains(URI.create("http://example.com/ns#CustomError"));
    }

    @Test
    @DisplayName("Error should have getAbout and setAbout methods")
    void errorShouldHaveAboutMethods() throws Exception {
        // Given an Error with an about URI
        Error error = new Error();
        error.setAbout(URI.create("http://example.com/errors/1"));
        error.setMessage("Error with URI");
        error.setStatusCode("500");

        // When serializing and deserializing
        Model model = JenaModelHelper.createJenaModel(new Object[]{error});
        logModel(model);

        Error[] errors = JenaModelHelper.unmarshal(model, Error.class);

        // Then the about URI should be preserved
        assertThat(errors).hasSize(1);
        assertThat(errors[0].getAbout()).isEqualTo(URI.create("http://example.com/errors/1"));
    }

    private void logModel(Model model) {
        if (log.isDebugEnabled()) {
            StringWriter writer = new StringWriter();
            RDFDataMgr.write(writer, model, RDFFormat.TURTLE_PRETTY);
            log.debug("Model:\n{}", writer.toString());
        }
    }
}
