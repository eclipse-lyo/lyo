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

import static org.assertj.core.api.Assertions.assertThat;

import java.io.InputStream;
import java.net.URI;
import java.util.Set;
import javax.xml.namespace.QName;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFLanguages;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.server.ResourceConfig;
import org.glassfish.jersey.servlet.ServletContainer;
import org.glassfish.jersey.test.DeploymentContext;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.ServletDeploymentContext;
import org.glassfish.jersey.test.TestProperties;
import org.glassfish.jersey.test.grizzly.GrizzlyWebTestContainerFactory;
import org.glassfish.jersey.test.spi.TestContainerException;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import jakarta.ws.rs.core.Response;

/**
 * Grizzly-based server tests for the OSLC Error class.
 * These tests verify that Error resources can be properly serialized and 
 * deserialized over HTTP, including extended properties.
 * 
 * @see <a href="https://github.com/oslc-op/oslc-specs/issues/466#issuecomment-762351427">Issue reference</a>
 * @since 7.0.0
 */
public class ErrorGrizzlyServerTest extends JerseyTest {

    private static final String DCTERMS_NAMESPACE = "http://purl.org/dc/terms/";

    @Override
    protected TestContainerFactory getTestContainerFactory() throws TestContainerException {
        return new GrizzlyWebTestContainerFactory();
    }

    @Override
    protected DeploymentContext configureDeployment() {
        return ServletDeploymentContext.builder(configure())
                .servlet(new ServletContainer(configure()))
                .build();
    }

    @Override
    protected void configureClient(final ClientConfig config) {
        configureProviders().forEach(config::register);
    }

    @Override
    protected ResourceConfig configure() {
        enable(TestProperties.LOG_TRAFFIC);
        enable(TestProperties.DUMP_ENTITY);

        return new ResourceConfig(ErrorTestResource.class)
                .registerClasses(configureProviders());
    }

    private static Set<Class<?>> configureProviders() {
        return JenaProvidersRegistry.getProviders();
    }

    @Test
    @DisplayName("Server endpoint should return basic error in Turtle format")
    void serverShouldReturnBasicErrorInTurtle() {
        Response response = target("/error-test/basic")
                .request(OslcMediaType.TEXT_TURTLE_TYPE)
                .get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SERVER_ERROR);

        Error[] errors = JenaModelHelper.unmarshal(
                parseRdfModel(response, RDFLanguages.TURTLE.getName()), Error.class);

        assertThat(errors).hasSize(1);
        assertThat(errors[0].getMessage()).isEqualTo("A basic error message");
        assertThat(errors[0].getStatusCode()).isEqualTo("500");
    }

    @Test
    @DisplayName("Server endpoint should return error with extended error info")
    void serverShouldReturnErrorWithExtendedError() {
        Response response = target("/error-test/with-extended")
                .request(OslcMediaType.TEXT_TURTLE_TYPE)
                .get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.CLIENT_ERROR);

        Error[] errors = JenaModelHelper.unmarshal(
                parseRdfModel(response, RDFLanguages.TURTLE.getName()), Error.class);

        assertThat(errors).hasSize(1);
        assertThat(errors[0].getExtendedError()).isNotNull();
        assertThat(errors[0].getExtendedError().getMoreInfo())
                .isEqualTo(URI.create("http://example.com/help/error-400"));
        assertThat(errors[0].getExtendedError().getHintHeight()).isEqualTo("300px");
        assertThat(errors[0].getExtendedError().getHintWidth()).isEqualTo("500px");
        assertThat(errors[0].getExtendedError().getRel()).isEqualTo("alternate");
    }

    @Test
    @DisplayName("Server endpoint should return error with dcterms:description in Turtle")
    void serverShouldReturnErrorWithDescriptionInTurtle() {
        Response response = target("/error-test/with-description")
                .request(OslcMediaType.TEXT_TURTLE_TYPE)
                .get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SERVER_ERROR);

        Error[] errors = JenaModelHelper.unmarshal(
                parseRdfModel(response, RDFLanguages.TURTLE.getName()), Error.class);

        assertThat(errors).hasSize(1);
        
        QName descriptionQName = new QName(DCTERMS_NAMESPACE, "description", "dcterms");
        assertThat(errors[0].getExtendedProperties()).containsKey(descriptionQName);
        assertThat(errors[0].getExtendedProperties().get(descriptionQName))
                .isEqualTo("Detailed stack trace: java.lang.NullPointerException at line 42");
    }

    @Test
    @DisplayName("Server endpoint should return error with dcterms:identifier")
    void serverShouldReturnErrorWithIdentifier() {
        Response response = target("/error-test/with-identifier")
                .request(OslcMediaType.TEXT_TURTLE_TYPE)
                .get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.CLIENT_ERROR);

        Error[] errors = JenaModelHelper.unmarshal(
                parseRdfModel(response, RDFLanguages.TURTLE.getName()), Error.class);

        assertThat(errors).hasSize(1);
        assertThat(errors[0].getStatusCode()).isEqualTo("404");
        
        QName identifierQName = new QName(DCTERMS_NAMESPACE, "identifier", "dcterms");
        assertThat(errors[0].getExtendedProperties()).containsKey(identifierQName);
        assertThat(errors[0].getExtendedProperties().get(identifierQName))
                .isEqualTo("ERR-404-RESOURCE");
    }

    @Test
    @DisplayName("Server endpoint should return error with all extensions in RDF/XML")
    void serverShouldReturnErrorWithAllExtensionsInRdfXml() {
        Response response = target("/error-test/with-all-extensions")
                .request(OslcMediaType.APPLICATION_RDF_XML_TYPE)
                .get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SERVER_ERROR);

        Error[] errors = JenaModelHelper.unmarshal(
                parseRdfModel(response, RDFLanguages.RDFXML.getName()), Error.class);

        assertThat(errors).hasSize(1);
        assertThat(errors[0].getMessage()).isEqualTo("Comprehensive error with all extensions");
        assertThat(errors[0].getStatusCode()).isEqualTo("500");
        
        // Check extended error
        assertThat(errors[0].getExtendedError()).isNotNull();
        assertThat(errors[0].getExtendedError().getMoreInfo())
                .isEqualTo(URI.create("http://example.com/help/500"));
        
        // Check extended properties
        QName descriptionQName = new QName(DCTERMS_NAMESPACE, "description", "dcterms");
        QName identifierQName = new QName(DCTERMS_NAMESPACE, "identifier", "dcterms");
        
        assertThat(errors[0].getExtendedProperties()).hasSize(2);
        assertThat(errors[0].getExtendedProperties().get(descriptionQName))
                .isEqualTo("Full stack trace information");
        assertThat(errors[0].getExtendedProperties().get(identifierQName))
                .isEqualTo("ERR-500-FULL");
        
        // Check custom type
        assertThat(errors[0].getTypes())
                .contains(URI.create("http://example.com/ns#InternalServerError"));
    }

    @Test
    @DisplayName("Server endpoint should return error with description in JSON-LD")
    void serverShouldReturnErrorWithDescriptionInJsonLd() {
        Response response = target("/error-test/with-description")
                .request(OslcMediaType.APPLICATION_JSON_LD_TYPE)
                .get();
        response.bufferEntity();

        assertThat(response.getStatusInfo().getFamily())
                .isEqualTo(Response.Status.Family.SERVER_ERROR);

        Error[] errors = JenaModelHelper.unmarshal(
                parseRdfModel(response, RDFLanguages.JSONLD.getName()), Error.class);

        assertThat(errors).hasSize(1);
        assertThat(errors[0].getMessage()).isEqualTo("Error with dcterms:description");
        
        QName descriptionQName = new QName(DCTERMS_NAMESPACE, "description", "dcterms");
        assertThat(errors[0].getExtendedProperties()).containsKey(descriptionQName);
    }

    private Model parseRdfModel(Response response, String lang) {
        InputStream inputStream = response.readEntity(InputStream.class);
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, inputStream, RDFLanguages.nameToLang(lang));
        return model;
    }
}
