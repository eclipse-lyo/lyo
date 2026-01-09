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
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.TimeZone;
import javax.xml.namespace.QName;
import org.apache.jena.datatypes.xsd.XSDDatatype;
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

	/**
	 * Test that a date-only value (xsd:date format) tagged as xsd:dateTime is
	 * successfully parsed as a Date object in non-strict mode, instead of returning
	 * an UnparseableLiteral. This addresses the DOORS interoperability issue
	 * where dates are sent in date-only format but with dateTime type annotation.
	 * <p>
	 * Issue: https://github.com/eclipse/lyo/issues/242
	 */
	@Test
	public void testDateOnlyValueWithDateTimeType() throws Exception {
		final Model m = ModelFactory.createDefaultModel();
		final Resource r = m.createResource("http://example.com/test/1",
				m.createResource(TestResource.TEST_RESOURCE_TYPE));
		// Add a date-only value with dateTime type (simulating DOORS behavior)
		r.addProperty(
				m.createProperty(DCTerms.NS, "created"),
				m.createTypedLiteral("2020-08-10", XSDDatatype.XSDdateTime.getURI()));

		final TestResource[] resources = JenaModelHelper.unmarshal(m, TestResource.class);
		assertEquals("Expected one resource", 1, resources.length);

		final TestResource resource = resources[0];
		final Map<QName, Object> extended = resource.getExtendedProperties();
		final Object createdValue = extended.get(new QName(DCTerms.NS, "created"));

		assertNotNull("Created property should not be null", createdValue);
		assertTrue("Expected Date instance for date-only value with dateTime type, but got: "
				+ createdValue.getClass().getName(), createdValue instanceof Date);

		// Verify the date is correctly parsed (2020-08-10)
		final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		cal.setTime((Date) createdValue);
		assertEquals("Year should be 2020", 2020, cal.get(Calendar.YEAR));
		assertEquals("Month should be August (7 in 0-indexed)", 7, cal.get(Calendar.MONTH));
		assertEquals("Day should be 10", 10, cal.get(Calendar.DAY_OF_MONTH));
	}

	/**
	 * Test that a full xsd:dateTime value (with time component) is still correctly
	 * parsed as a Date object. This ensures the date-only fallback fix does not
	 * affect normal dateTime processing.
	 */
	@Test
	public void testFullDateTimeValueStillParsesCorrectly() throws Exception {
		final Model m = ModelFactory.createDefaultModel();
		final Resource r = m.createResource("http://example.com/test/2",
				m.createResource(TestResource.TEST_RESOURCE_TYPE));
		// Add a full dateTime value with time component
		r.addProperty(
				m.createProperty(DCTerms.NS, "created"),
				m.createTypedLiteral("2020-08-10T14:30:45Z", XSDDatatype.XSDdateTime));

		final TestResource[] resources = JenaModelHelper.unmarshal(m, TestResource.class);
		assertEquals("Expected one resource", 1, resources.length);

		final TestResource resource = resources[0];
		final Map<QName, Object> extended = resource.getExtendedProperties();
		final Object createdValue = extended.get(new QName(DCTerms.NS, "created"));

		assertNotNull("Created property should not be null", createdValue);
		assertTrue("Expected Date instance for full dateTime value, but got: "
				+ createdValue.getClass().getName(), createdValue instanceof Date);

		// Verify the datetime is correctly parsed (2020-08-10T14:30:45Z)
		final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
		cal.setTime((Date) createdValue);
		assertEquals("Year should be 2020", 2020, cal.get(Calendar.YEAR));
		assertEquals("Month should be August (7 in 0-indexed)", 7, cal.get(Calendar.MONTH));
		assertEquals("Day should be 10", 10, cal.get(Calendar.DAY_OF_MONTH));
		assertEquals("Hour should be 14", 14, cal.get(Calendar.HOUR_OF_DAY));
		assertEquals("Minute should be 30", 30, cal.get(Calendar.MINUTE));
		assertEquals("Second should be 45", 45, cal.get(Calendar.SECOND));
	}
}
