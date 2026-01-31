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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.nio.charset.StandardCharsets;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedHashMap;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Resource;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;
import org.junit.Test;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

public class OslcRdfXmlProviderModelTest {

    @Test
    public void testReadComplexModel() throws Exception {
        OslcRdfXmlProvider provider = new OslcRdfXmlProvider();

        // RDF/XML representing:
        // <http://example.com/r1> <http://example.com/p1> "value" .
        // <http://example.com/r1> <http://example.com/p2> _:b1 .
        // _:b1 <http://example.com/p3> <http://example.com/r2> .

        String rdfXml =
            "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:ex=\"http://example.com/\">" +
            "  <rdf:Description rdf:about=\"http://example.com/r1\">" +
            "    <ex:p1>value</ex:p1>" +
            "    <ex:p2>" +
            "      <rdf:Description>" +
            "        <ex:p3 rdf:resource=\"http://example.com/r2\"/>" +
            "      </rdf:Description>" +
            "    </ex:p2>" +
            "  </rdf:Description>" +
            "</rdf:RDF>";

        InputStream inputStream = new ByteArrayInputStream(rdfXml.getBytes(StandardCharsets.UTF_8));

        Object result = provider.readFrom(
            (Class) Model.class,
            Model.class,
            new Annotation[]{},
            MediaType.valueOf("application/rdf+xml"),
            new MultivaluedHashMap<>(),
            inputStream
        );

        assertNotNull("Result should not be null", result);
        assertTrue("Result should be instance of Model", result instanceof Model);
        Model model = (Model) result;

        Resource r1 = model.createResource("http://example.com/r1");
        assertTrue("Model should contain r1", model.containsResource(r1));

        // Check p1 property
        assertTrue("r1 should have p1=value",
            model.contains(r1, model.createProperty("http://example.com/p1"), "value"));

        // Check p2 property (to blank node)
        // Since we can't easily guess the AnonId, we list statements
        assertTrue("r1 should have p2 property",
            r1.hasProperty(model.createProperty("http://example.com/p2")));

        Resource bNode = r1.getPropertyResourceValue(model.createProperty("http://example.com/p2"));
        assertNotNull("Blank node should exist", bNode);
        assertTrue("Blank node should be anonymous", bNode.isAnon());

        // Check blank node property p3 pointing to r2
        Resource r2 = model.createResource("http://example.com/r2");
        assertTrue("Blank node should point to r2 via p3",
            bNode.hasProperty(model.createProperty("http://example.com/p3"), r2));

        // Check total size
        assertEquals("Model should have 3 statements", 3, model.size());
    }
}
