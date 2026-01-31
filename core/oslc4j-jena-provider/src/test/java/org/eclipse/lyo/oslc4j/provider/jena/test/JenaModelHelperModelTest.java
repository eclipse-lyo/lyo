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

import org.apache.jena.rdf.model.AnonId;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class JenaModelHelperModelTest {

    @Test
    public void testUnmarshalComplexModel() throws Exception {
        Model m = ModelFactory.createDefaultModel();

        // Define properties
        Property p1 = m.createProperty("http://example.com/p1");
        Property p2 = m.createProperty("http://example.com/p2");
        Property p3 = m.createProperty("http://example.com/p3");

        // Define resources
        Resource r1 = m.createResource("http://example.com/r1");
        Resource r2 = m.createResource("http://example.com/r2");

        // Define blank nodes
        Resource b1 = m.createResource(AnonId.create("b1"));
        Resource b2 = m.createResource(AnonId.create("b2"));

        // Statements
        // URI -> URI
        m.add(r1, p1, r2);

        // URI -> Blank
        m.add(r1, p2, b1);

        // Blank -> Literal
        m.add(b1, p1, "literal value");

        // Blank -> URI
        m.add(b1, p2, r2);

        // Blank -> Blank
        m.add(b1, p3, b2);

        // Circular URI -> URI
        m.add(r2, p1, r1);

        // Circular Blank -> URI -> Blank (indirect)
        m.add(b2, p1, r1);

        // Unmarshal
        Model[] models = JenaModelHelper.unmarshal(m, Model.class);
        assertNotNull(models);
        assertEquals(1, models.length);
        Model result = models[0];

        // Verification
        // Since we are returning the SAME model (or at least checking containment), we verify structure.
        assertEquals("Should return the same model instance", m, result);

        assertTrue(result.contains(r1, p1, r2));
        assertTrue(result.contains(r1, p2, b1));
        assertTrue(result.contains(b1, p1, "literal value"));
        assertTrue(result.contains(b1, p2, r2));
        assertTrue(result.contains(b1, p3, b2));
        assertTrue(result.contains(r2, p1, r1));
        assertTrue(result.contains(b2, p1, r1));

        assertEquals("Model size should match", 7, result.size());
    }

    @Test
    public void testUnmarshalEmptyModel() throws Exception {
        Model m = ModelFactory.createDefaultModel();

        Model[] models = JenaModelHelper.unmarshal(m, Model.class);
        assertNotNull(models);
        assertEquals(1, models.length);
        assertTrue(models[0].isEmpty());
    }
}
