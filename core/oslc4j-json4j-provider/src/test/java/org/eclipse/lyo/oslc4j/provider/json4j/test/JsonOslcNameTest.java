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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.oslc4j.provider.json4j.test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.xml.namespace.QName;

import org.apache.wink.json4j.JSONArray;
import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.apache.wink.json4j.OrderedJSONObject;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.json4j.OslcRdfJsonProvider;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.EmptyNameResource;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.TestResource;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.UnnamedResource;
import org.junit.Assert;
import org.junit.Test;

/**
 * Tests Json working with different combinations of OslcName annotation.
 *
 * @author Fabio Negrello
 *
 */
@SuppressWarnings("deprecation")
public class JsonOslcNameTest {

	private final String NAME_LOCAL_PART = "name";
	private final String DESCRIPTION_LOCAL_PART = "description";
	private final String TEST1_URL = "http://test1.oslc4j.com#";
	private final String TEST2_URL = "http://test2.oslc4j.com#";

	/**
	 * Checks that OslcName annotation with empty string does not add RDF type
	 * to the resource.
	 *
	 * @throws Exception
	 */
	@Test
	public void testJenaOslcNameEmptyString() throws Exception {
		EmptyNameResource resource = createResource(EmptyNameResource.class);
		final OslcRdfJsonProvider oslcRdfJsonProvider = new OslcRdfJsonProvider();
		JSONObject jsonObject = getJSONObject(resource, oslcRdfJsonProvider);
		JSONArray rdfTypes = jsonObject.getJSONArray("rdf:type");
		Assert.assertTrue("Model should not contain RDF type as OslcName is empty", rdfTypes.isEmpty());
	}

	private JSONObject getJSONObject(Object resource, final OslcRdfJsonProvider oslcRdfJsonProvider)
			throws IOException, WebApplicationException, JSONException {
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		oslcRdfJsonProvider.writeTo(resource, resource.getClass(), resource.getClass(), new Annotation[0],
				MediaType.APPLICATION_JSON_TYPE, null, outputStream);
		ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
		JSONObject jsonObject = new JSONObject(inputStream);
		return jsonObject;
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
		final OslcRdfJsonProvider oslcRdfJsonProvider = new OslcRdfJsonProvider();
		JSONObject jsonObject = getJSONObject(resource, oslcRdfJsonProvider);
		JSONArray rdfTypes = jsonObject.getJSONArray("rdf:type");
		Assert.assertEquals("Model should contain only added RDF type", rdfTypes.size(), 1);
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
		final OslcRdfJsonProvider oslcRdfJsonProvider = new OslcRdfJsonProvider();
		JSONObject jsonObject = getJSONObject(resource, oslcRdfJsonProvider);
		JSONArray rdfTypes = jsonObject.getJSONArray("rdf:type");
		Assert.assertEquals("Model should contain default RDF type", rdfTypes.size(), 1);
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
		String typeToAdd = "http://about.oslc.test/addedType";
		resource.getTypes().add(URI.create(typeToAdd));
		final OslcRdfJsonProvider oslcRdfJsonProvider = new OslcRdfJsonProvider();
		JSONObject jsonObject = getJSONObject(resource, oslcRdfJsonProvider);
		JSONArray rdfTypes = jsonObject.getJSONArray("rdf:type");
		Assert.assertFalse("Model should contain RDF types", rdfTypes.isEmpty());
		verifyRDFTypes(new String[] { typeToAdd, TestResource.TEST_NAMESPACE + "UnnamedResource" }, rdfTypes);
	}

	private void verifyRDFTypes(String[] expectedRDFTypes, JSONArray rdfTypes) throws JSONException {
		List<String> actualRdfTypesList = new ArrayList<String>();
		for (Object node : rdfTypes) {
			OrderedJSONObject obj = (OrderedJSONObject) node;
			String type = obj.values().iterator().next().toString();
			actualRdfTypesList.add(type);
		}
		for (String expectedRdfType : expectedRDFTypes) {
			Assert.assertTrue("Resource should contain RDF type " + expectedRdfType,
					actualRdfTypesList.contains(expectedRdfType));
		}
	}

	/**
	 * Creates a new instance adding some test values.
	 *
	 * @param resource
	 *			  class.
	 * @return new instance
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
