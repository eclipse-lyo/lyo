/*
 * Copyright (c) 2026 Contributors to the Eclipse Foundation
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.lang.annotation.Annotation;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedHashMap;

import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.junit.Test;

/**
 * Regression tests for {@code AbstractOslcRdfXmlProvider#getRdfWriter(String, Model)}: on
 * Jena 6+, {@code model.getWriter("RDF/XML")} is unreliable and can silently return abbreviated
 * (typed-node) output instead of the plain writer. These tests verify the actual RDF/XML text
 * produced for each media type, since both writers emit triple-equivalent models - only the
 * syntactic form (which these tests check) differs.
 */
public class OslcRdfXmlProviderWriteTest {

    @Test
    public void testApplicationRdfXmlUsesPlainNonAbbreviatedWriter() throws Exception {
        OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

        TestResource resource = new TestResource(URI.create("http://example.com/resource1"));
        resource.setAproperty("value1");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        provider.writeTo(
            resource,
            TestResource.class,
            TestResource.class,
            new Annotation[]{},
            MediaType.valueOf("application/rdf+xml"),
            new MultivaluedHashMap<>(),
            outputStream
        );

        String rdfXml = outputStream.toString(StandardCharsets.UTF_8);

        // The plain ("RDF/XML") writer always wraps every subject in a literal
        // <rdf:Description> element with an explicit <rdf:type>, never a typed-node
        // shorthand element (e.g. <ex:Test rdf:about="...">) or nested resources.
        assertTrue("Plain RDF/XML writer should use <rdf:Description>, got: " + rdfXml,
            rdfXml.contains("<rdf:Description"));
        assertTrue("Plain RDF/XML writer should express the type as an explicit rdf:type statement",
            rdfXml.contains(TestResource.TEST_RESOURCE_TYPE));
    }

    @Test
    public void testApplicationXmlUsesAbbreviatedWriter() throws Exception {
        OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

        TestResource resource = new TestResource(URI.create("http://example.com/resource1"));
        resource.setAproperty("value1");

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        provider.writeTo(
            resource,
            TestResource.class,
            TestResource.class,
            new Annotation[]{},
            MediaType.valueOf("application/xml"),
            new MultivaluedHashMap<>(),
            outputStream
        );

        String rdfXml = outputStream.toString(StandardCharsets.UTF_8);

        // application/xml is intentionally always abbreviated (unaffected by this fix);
        // asserting this documents the existing contrast and guards against a regression
        // in the other direction.
        assertFalse("Abbreviated writer should not use the plain <rdf:Description> form",
            rdfXml.contains("<rdf:Description"));
    }
}
