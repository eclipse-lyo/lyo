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
import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.riot.RiotException;
import org.apache.jena.sparql.vocabulary.FOAF;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;
import org.eclipse.lyo.oslc4j.provider.jena.AbstractOslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResourceWithLiterals;
import org.junit.Before;
import org.junit.Test;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.namespace.QName;
import java.io.InputStream;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;

import static org.junit.Assert.*;

public class XmlLiteralTest {

    private final QName FIRST_NAME_PROP = new QName(FOAF.NS, "firstName");

    @Before
    public void before() {
        System.getProperties().remove(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES);
    }

    /* ROUND-TRIPPING TESTS */

    @Test
    public void roundtripTestXmlDatatypeRDF() throws DatatypeConfigurationException, OslcCoreApplicationException,
        InvocationTargetException, IllegalAccessException {
        final Model diskModel = readModel("/xml_literals/datatype_rdf.rdf", RDFLanguages.strLangRDFXML);

        assertModelUnmarshalsAndBackIsomorphically(diskModel, TestResourceWithLiterals.class);
    }

    @Test
    public void roundtripTestXmlParsetypeWithAnnotatedLiteralProperty() throws DatatypeConfigurationException,
        OslcCoreApplicationException, InvocationTargetException, IllegalAccessException {
        final Model diskModel = readModel("/xml_literals/parsetype_annot.rdf", RDFLanguages.strLangRDFXML);

        assertModelUnmarshalsAndBackIsomorphically(diskModel, TestResourceWithLiterals.class);
    }

    @Test
    public void roundtripTestXmlParsetypeWithAnnotatedStringProperty() throws DatatypeConfigurationException,
        OslcCoreApplicationException, InvocationTargetException, IllegalAccessException {
        final Model diskModel = readModel("/xml_literals/parsetype_annot_string.rdf", RDFLanguages.strLangRDFXML);

        assertModelUnmarshalsAndBackIsomorphically(diskModel, TestResourceWithLiterals.class);
    }

    /* INVALID LITERAL TESTS */

    /**
     * We only expect warnings here
     */
    @Test
    public void invalidLiteralTest_NoRoot() throws DatatypeConfigurationException,
        OslcCoreApplicationException, InvocationTargetException, IllegalAccessException {
        final Model diskModel = readModel("/xml_literals/invalid_noroot.ttl", RDFLanguages.strLangTurtle);

        assertModelUnmarshalsAndBackIsomorphically(diskModel, TestResourceWithLiterals.class);
    }

    /**
     * We only expect warnings here
     */
    @Test(expected = DatatypeFormatException.class)
    public void invalidLiteralTest_NoEscape() throws DatatypeConfigurationException,
        OslcCoreApplicationException, InvocationTargetException, IllegalAccessException {
        final Model diskModel = readModel("/xml_literals/invalid_noescape.ttl", RDFLanguages.strLangTurtle);

        assertModelUnmarshalsAndBackIsomorphically(diskModel, TestResourceWithLiterals.class);
    }

    /**
     * Not escaping & turns XML invalid. NB! Unescaped & in Turtle will get escaped when you use libs to convert to
     * RDF/XML.
     */
    @Test(expected = RiotException.class)
    public void invalidLiteralTest_BadXml() {
        final Model diskModel = readModel("/xml_literals/invalid_badxml.rdf", RDFLanguages.strLangTurtle);

        final TestResourceWithLiterals[] testResources = JenaModelHelper.unmarshal(diskModel,
            TestResourceWithLiterals.class);
    }


    /* BASE TYPE TESTS */

    @Test
    public void literalTestTurtleXSD() {
        final Model m = readModel("/xml_literals/xsd_xmlliteral.ttl", RDFLanguages.strLangTurtle);

        final TestResourceWithLiterals[] testResources = JenaModelHelper.unmarshal(m, TestResourceWithLiterals.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResourceWithLiterals resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof BaseDatatype.TypedValue);
        assertEquals("http://www.w3.org/2001/XMLSchema#XMLLiteral",
            ((BaseDatatype.TypedValue) firstName).datatypeURI);
    }

    @Test
    public void literalTestTurtleRDF() {
        final Model m = readModel("/xml_literals/rdf_xmlliteral.ttl", RDFLanguages.strLangTurtle);

        final TestResourceWithLiterals[] testResources = JenaModelHelper.unmarshal(m, TestResourceWithLiterals.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResourceWithLiterals resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof XMLLiteral);
        assertEquals("<span>David &amp; Mary</span>", ((XMLLiteral) firstName).getValue());
    }

    @Test
    public void literalTestXmlDatatypeXSD() {
        final Model m = readModel("/xml_literals/datatype_xsd.rdf", RDFLanguages.strLangRDFXML);

        final TestResourceWithLiterals[] testResources = JenaModelHelper.unmarshal(m, TestResourceWithLiterals.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResourceWithLiterals resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof BaseDatatype.TypedValue);
        assertEquals("http://www.w3.org/2001/XMLSchema#XMLLiteral",
            ((BaseDatatype.TypedValue) firstName).datatypeURI);
    }


    @Test
    public void literalTestXmlDatatypeRDF() {
        final Model m = readModel("/xml_literals/datatype_rdf.rdf", RDFLanguages.strLangRDFXML);

        final TestResourceWithLiterals[] testResources = JenaModelHelper.unmarshal(m, TestResourceWithLiterals.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResourceWithLiterals resource = testResources[0];
        Object firstName = resource.getExtendedProperties().get(FIRST_NAME_PROP);

        assertTrue(firstName instanceof XMLLiteral);
        assertEquals("<span>David &amp; Mary</span>", ((XMLLiteral) firstName).getValue());
    }

    @Test
    public void literalTestXmlParsetype() {
        final Model m = readModel("/xml_literals/parsetype.rdf", RDFLanguages.strLangRDFXML);

        final TestResourceWithLiterals[] testResources = JenaModelHelper.unmarshal(m, TestResourceWithLiterals.class);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResourceWithLiterals resource = testResources[0];
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

    /*HELPERS*/

    private void assertModelUnmarshalsAndBackIsomorphically(Model diskModel, Class<TestResourceWithLiterals> klass)
        throws DatatypeConfigurationException, IllegalAccessException, InvocationTargetException,
        OslcCoreApplicationException {
        final TestResourceWithLiterals[] testResources = JenaModelHelper.unmarshal(diskModel, klass);
        assertEquals("Expected only one TestResource resource", 1, testResources.length);
        final TestResourceWithLiterals resource = testResources[0];

        Model lyoModel = JenaModelHelper.createJenaModel(new Object[]{resource});

        assertTrue("Models should match: " + showModels(diskModel, lyoModel), diskModel.isIsomorphicWith(lyoModel));
    }

    private String showModels(Model expected, Model resulting) {
        return String.format("EXPECTED model:\n\n%s\n" +
            "=======================================================\n\n"
            + "RESULTING model:\n\n%s\n", prettyPrintModel(expected), prettyPrintModel(resulting));
    }

    private String prettyPrintModel(Model model) {
        StringWriter writer = new StringWriter(4096);
        RDFDataMgr.write(writer, model, RDFFormat.TRIG_PRETTY);
        return writer.toString();
    }

}
