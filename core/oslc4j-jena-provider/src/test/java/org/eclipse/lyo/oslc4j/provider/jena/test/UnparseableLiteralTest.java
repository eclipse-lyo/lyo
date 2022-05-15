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

import java.io.InputStream;
import java.net.URI;
import java.util.Map;
import javax.xml.namespace.QName;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.DCTerms;
import org.eclipse.lyo.oslc4j.core.UnparseableLiteral;
import org.eclipse.lyo.oslc4j.provider.jena.AbstractOslcRdfXmlProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class UnparseableLiteralTest {
	@Before
	public void before() {
		System.setProperty(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES, "false");
	}

	@After
	public void after() {
		System.getProperties().remove(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES);
	}

	@Test
	public void testParseTheUnparseable() throws Exception {
		final InputStream is = ServiceProviderTest.class.getResourceAsStream("/badliterals.ttl");
		assertNotNull("Could not read file: badliterals.ttl", is);
		final Model m = ModelFactory.createDefaultModel();
		m.read(is, null, "TURTLE");

		final TestResource[] testResources = JenaModelHelper.unmarshal(m, TestResource.class);
		assertEquals("Expected only one TestResource resource", 1, testResources.length);

		final TestResource resource = testResources[0];
		verifyUnparseable(resource, new QName(DCTerms.NS, "identifier"),
				"thirteen", "http://www.w3.org/2001/XMLSchema#int");
		verifyUnparseable(resource, new QName(TestResource.TEST_NAMESPACE, "age"),
				"", "http://www.w3.org/2001/XMLSchema#long");
		verifyUnparseable(resource, new QName(DCTerms.NS, "modified"),
				"yesterday", "http://www.w3.org/2001/XMLSchema#dateTime");
	}

	@Test
	public void serializeUnparseable() throws Exception {
		final TestResource resource = new TestResource();
		resource.setAbout(new URI("http://example.com/bugs/8234"));

		final UnparseableLiteral age =
		   	new UnparseableLiteral("", "http://www.w3.org/2001/XMLSchema#dateTime");
		resource.getExtendedProperties().put(new QName(TestResource.TEST_NAMESPACE, "age"), age);

		final UnparseableLiteral modified =
		   	new UnparseableLiteral("today", "http://www.w3.org/2001/XMLSchema#dateTime");
		resource.getExtendedProperties().put(new QName(DCTerms.NS, "modified"), modified);

		final Model m = JenaModelHelper.createJenaModel(new Object[] { resource });
		final Resource serialized = m.getResource("http://example.com/bugs/8234");

		final Statement ageStatement = serialized.getProperty(m.createProperty(TestResource.TEST_NAMESPACE + "age"));
		assertEquals("Unexpected value for ex:age", "", ageStatement.getString());
		// Since Jena 3 an uninitialised literal is an empty string, not a null anymore
//		assertNull(ageStatement.getLiteral().getDatatypeURI());

		final Statement modifiedStatement = serialized.getProperty(DCTerms.modified);
		assertEquals("Unexpected value for dcterms:modified", "today", modifiedStatement.getString());
		// Since Jena 3 an uninitialised literal is an empty string, not a null anymore
//		assertNull(modifiedStatement.getLiteral().getDatatypeURI());
	}

	private void verifyUnparseable(
			TestResource resource,
			QName property,
			String expectedValue,
			String expectedType)
	{
		final Map<QName,Object> extended = resource.getExtendedProperties();
		final Object o = extended.get(property);
		assertTrue("Expected UnparseableLiteral for property " + property,
				o instanceof UnparseableLiteral);
		final UnparseableLiteral unparseable = (UnparseableLiteral) o;
		assertEquals("Incorrect raw value", expectedValue, unparseable.getRawValue());
		assertEquals("Incorrect datatype", expectedType, unparseable.getDatatype());
	}
}
