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

import org.apache.jena.datatypes.BaseDatatype;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.sparql.vocabulary.FOAF;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;
import org.eclipse.lyo.oslc4j.provider.jena.AbstractOslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResourceWithXMLLiteral;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.namespace.QName;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import static org.junit.Assert.*;

public class XmlLiteralTest {

    private final QName FIRST_NAME_PROP = new QName(FOAF.NS, "firstName");

    @Before
    public void before() {
        System.getProperties().remove(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES);
//		System.setProperty(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES, "false");
    }

    @After
    public void after() {
        System.getProperties().remove(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES);
    }

    /* ROUND-TRIPPING TESTS */

    @Test
    public void roundtripTestXmlDatatypeRDF() throws DatatypeConfigurationException, OslcCoreApplicationException,
        InvocationTargetException, IllegalAccessException {
        final Model diskModel = readModel("/xml_literals/datatype_rdf.rdf", RDFLanguages.strLangRDFXML);

        final TestResource[] testResources = JenaModelHelper.unmarshal(diskModel, TestResource.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResource resource = testResources[0];

        Model lyoModel = JenaModelHelper.createJenaModel(new Object[]{resource});

        assertTrue(diskModel.isIsomorphicWith(lyoModel));
    }

    @Test
    public void roundtripTestXmlParsetypeWithAnnotatedProperty() throws DatatypeConfigurationException,
        OslcCoreApplicationException, InvocationTargetException, IllegalAccessException {
        final Model diskModel = readModel("/xml_literals/parsetype_annot.rdf", RDFLanguages.strLangRDFXML);

        final TestResourceWithXMLLiteral[] testResources = JenaModelHelper.unmarshal(diskModel, TestResourceWithXMLLiteral.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResourceWithXMLLiteral resource = testResources[0];

        Model lyoModel = JenaModelHelper.createJenaModel(new Object[]{resource});

        assertTrue(diskModel.isIsomorphicWith(lyoModel));
    }


    /* BASE TYPE TESTS */

    @Test
    public void literalTestTurtleXSD() {
        final Model m = readModel("/xml_literals/xsd_xmlliteral.ttl", RDFLanguages.strLangTurtle);

        final TestResource[] testResources = JenaModelHelper.unmarshal(m, TestResource.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResource resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof BaseDatatype.TypedValue);
        assertEquals("http://www.w3.org/2001/XMLSchema#XMLLiteral",
            ((BaseDatatype.TypedValue) firstName).datatypeURI);
    }

    @Test
    public void literalTestTurtleRDF() {
        final Model m = readModel("/xml_literals/rdf_xmlliteral.ttl", RDFLanguages.strLangTurtle);

        final TestResource[] testResources = JenaModelHelper.unmarshal(m, TestResource.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResource resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof XMLLiteral);
        assertEquals("<span>David &amp; Mary</span>", ((XMLLiteral) firstName).getValue());
    }

    @Test
    public void literalTestXmlDatatypeXSD() {
        final Model m = readModel("/xml_literals/datatype_xsd.rdf", RDFLanguages.strLangRDFXML);

        final TestResource[] testResources = JenaModelHelper.unmarshal(m, TestResource.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResource resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof BaseDatatype.TypedValue);
        assertEquals("http://www.w3.org/2001/XMLSchema#XMLLiteral",
            ((BaseDatatype.TypedValue) firstName).datatypeURI);
    }


    @Test
    public void literalTestXmlDatatypeRDF() {
        final Model m = readModel("/xml_literals/datatype_rdf.rdf", RDFLanguages.strLangRDFXML);

        final TestResource[] testResources = JenaModelHelper.unmarshal(m, TestResource.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResource resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof XMLLiteral);
        assertEquals("<span>David &amp; Mary</span>", ((XMLLiteral) firstName).getValue());
    }

    @Test
    public void literalTestXmlParsetype() {
        final Model m = readModel("/xml_literals/parsetype.rdf", RDFLanguages.strLangRDFXML);

        final TestResource[] testResources = JenaModelHelper.unmarshal(m, TestResource.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResource resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof XMLLiteral);
        assertEquals("<span>David &amp; Mary</span>", ((XMLLiteral) firstName).getValue());
    }

    private Model readModel(String filePath, String lang) {
        final InputStream is = ServiceProviderTest.class.getResourceAsStream(filePath);
        assertNotNull("Could not read file: " + filePath, is);
        final Model m = ModelFactory.createDefaultModel();
        m.read(is, null, lang);
        return m;
    }
}
