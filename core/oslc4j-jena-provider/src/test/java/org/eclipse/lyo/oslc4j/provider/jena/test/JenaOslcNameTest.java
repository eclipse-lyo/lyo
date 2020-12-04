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

import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.TEST1_URL;
import static org.eclipse.lyo.oslc4j.core.test.customnamespace.CustomNamespaceConstants.TEST2_URL;
import static org.junit.Assert.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.EmptyNameResource;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.TestResource;
import org.eclipse.lyo.oslc4j.provider.jena.test.resources.UnnamedResource;
import org.junit.Assert;
import org.junit.Test;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.vocabulary.RDF;

/**
 * Tests Jena working with different combinations of OslcName annotation.
 * 
 * @author Fabio Negrello
 * 
 */
public class JenaOslcNameTest {

	private final String NAME_LOCAL_PART = "name";
	private final String DESCRIPTION_LOCAL_PART = "description";

	/**
	 * Checks that OslcName annotation with empty string does not add RDF type
	 * to the resource.
	 * 
	 * @throws Exception
	 */
	@Test
	public void testJenaOslcNameEmptyString() throws Exception {
		EmptyNameResource resource = createResource(EmptyNameResource.class);
		Model model = JenaModelHelper.createJenaModel(new Object[] { resource });
		List<RDFNode> rdfTypes = model.listObjectsOfProperty(RDF.type).toList();
		assertTrue("Model should not contain RDF type as OslcName is empty", rdfTypes.isEmpty());
	}

	/**
	 * Checks that OslcName annotation with empty string does not add default
	 * RDF type to the resource but adds the ones specified by addTypes method.
	 * 
	 * @throws Exception
	 */
	@Test
	public void testJenaOslcNameEmptyStringAndOtherTypes() throws Exception {
		EmptyNameResource resource = createResource(EmptyNameResource.class);
		String typeToAdd = "http://about.oslc.test/addedType";
		resource.getTypes().add(URI.create(typeToAdd));
		Model model = JenaModelHelper.createJenaModel(new Object[] { resource });
		List<RDFNode> rdfTypes = model.listObjectsOfProperty(RDF.type).toList();
		assertEquals("Model should contain only added RDF type", rdfTypes.size(), 1);
		verifyRDFTypes(new String[] { typeToAdd }, rdfTypes);
	}

	/**
	 * Checks that OslcName annotation adds RDF type to the resource.
	 * 
	 * @throws Exception
	 */
	@Test
	public void testJenaRegularOslcName() throws Exception {
		TestResource resource = createResource(TestResource.class);
		Model model = JenaModelHelper.createJenaModel(new Object[] { resource });
		List<RDFNode> rdfTypes = model.listObjectsOfProperty(RDF.type).toList();
		assertFalse("Model should contain RDF types", rdfTypes.isEmpty());
		verifyRDFTypes(new String[] { TestResource.TEST_NAMESPACE + "Test" }, rdfTypes);
	}

	/**
	 * Checks that the absence of OslcName annotation adds default RDF type to
	 * the resource.
	 * 
	 * @throws Exception
	 */
	@Test
	public void testJenaDefaultOslcName() throws Exception {
		UnnamedResource resource = createResource(UnnamedResource.class);
		Model model = JenaModelHelper.createJenaModel(new Object[] { resource });
		List<RDFNode> rdfTypes = model.listObjectsOfProperty(RDF.type).toList();
		assertFalse("Model should contain RDF types", rdfTypes.isEmpty());
		verifyRDFTypes(new String[] { TestResource.TEST_NAMESPACE + "UnnamedResource" }, rdfTypes);
	}

	private void verifyRDFTypes(String[] expectedRDFTypes, List<RDFNode> actualRDFTypes) {
		List<String> actualRdfTypesList = new ArrayList<String>();
		for (RDFNode node : actualRDFTypes) {
			actualRdfTypesList.add(node.toString());
		}
		for (String expectedRdfType : expectedRDFTypes) {
			assertTrue("Resource should contain RDF type " + expectedRdfType,
					actualRdfTypesList.contains(expectedRdfType));
		}
	}

	/**
	 * Creates a new instance adding some test values.
	 * 
	 */
	private <T extends AbstractResource> T createResource(Class<T> resourceClass) throws Exception {
		T resource = resourceClass.newInstance();
		URI resourceAbout = URI.create("http://about.oslc.test/001");
		resource.setAbout(resourceAbout);
		QName qName = new QName(TEST1_URL, NAME_LOCAL_PART);
		resource.getExtendedProperties().put(qName, "Some Name");
		qName = new QName(TEST2_URL, DESCRIPTION_LOCAL_PART);
		resource.getExtendedProperties().put(qName, "Any Description");
		return resource;
	}

}
