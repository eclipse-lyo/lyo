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
package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.junit.Test;

public class TypesTest {
    private static final String TEST_RESOURCE = "http://example.com/resources/testResource";
    private static final String DECIMAL_PROPERTY = TestResource.TEST_NAMESPACE + "decimalProp";
    private static final String BOOLEAN_PROPERTY = TestResource.TEST_NAMESPACE + "booleanProp";

    /**
     * Tests that BigDecimal is always returned in extended properties for
     * xsd:decimal, even for small numbers.
     *
     * @throws Exception
     *			   on test errors
     */
    @Test
    public void testNoDecimalCannonicolization() throws Exception {
        Model m = ModelFactory.createDefaultModel();
        Resource r =
                m.createResource(TEST_RESOURCE, m.createResource(TestResource.TEST_RESOURCE_TYPE));
        BigDecimal testDecimalValue = new BigDecimal(2700);
        r.addLiteral(m.createProperty(DECIMAL_PROPERTY), m.createTypedLiteral(testDecimalValue));

        TestResource[] resourceArray = JenaModelHelper.unmarshal(m, TestResource.class);
        assertEquals("Expected to find one resource", 1, resourceArray.length);

        TestResource any = resourceArray[0];
        assertEquals(
                "Expected exactly one extended property", 1, any.getExtendedProperties().size());

        Object prop = any.getExtendedProperties().values().iterator().next();
        assertTrue("Property is not a BigDecimal", prop instanceof BigDecimal);
        assertEquals("Decimal is not expected value", testDecimalValue, prop);
    }

    /**
     * Tests that DatatypeFormatException is thrown for invalid literal values like
     *
     * <pre>{@code <ex:booleanProperty rdf:datatype="http://www.w3.org/2001/XMLSchema#boolean">invalid</ex:booleanProperty> }</pre>
     *
     * in extended property values.
     */
    @Test(expected = DatatypeFormatException.class)
    public void testInvalidBoolean() throws Exception {
        Model m = ModelFactory.createDefaultModel();
        Resource r =
                m.createResource(TEST_RESOURCE, m.createResource(TestResource.TEST_RESOURCE_TYPE));
        r.addLiteral(
                m.createProperty(BOOLEAN_PROPERTY),
                m.createTypedLiteral("invalid", XSDDatatype.XSDboolean.getURI()));
        JenaModelHelper.unmarshal(m, TestResource.class);
    }
}
