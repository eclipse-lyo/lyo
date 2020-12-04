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
package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.ws.rs.core.MediaType;

import org.eclipse.lyo.oslc4j.core.model.AllowedValues;
import org.eclipse.lyo.oslc4j.core.model.Property;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.provider.jena.OslcTurtleProvider;
import org.junit.Test;

public class ShapeTest {
	@Test
	public void allowedAndDefaultValuesTest() throws Exception {
		ResourceShape s = readResourceShape("/allowedValues.ttl");
		assertNotNull("Shape is null", s);
		
		Property allowedValuesUriProperty = s.getProperty(new URI("http://example.com/shapes/allowedValuesUriProperty"));
		assertNotNull("http://example.com/shapes/allowedValuesUriProperty property not in shape", s);
		
		Collection<?> uriAllowedValues = allowedValuesUriProperty.getAllowedValuesCollection();
		assertEquals("Wrong number of allowedValues for allowedValuesUriProperty property", 2, uriAllowedValues.size());
		assertTrue("uri1 not in allowed values", uriAllowedValues.contains(new URI("http://example.com/ns#uri1")));
		assertTrue("uri2 not in allowed values", uriAllowedValues.contains(new URI("http://example.com/ns#uri2")));
		
		assertEquals("Default values should be uri1", new URI("http://example.com/ns#uri1"), allowedValuesUriProperty.getDefaultValueObject());

		Property allowedValuesStringProperty = s.getProperty(new URI("http://example.com/shapes/allowedValuesStringProperty"));
		assertNotNull("http://example.com/shapes/allowedValuesStringProperty property not in shape", s);

		Collection<?> stringAllowedValues = allowedValuesStringProperty.getAllowedValuesCollection();
		assertEquals("Wrong number of allowedValues for allowedValuesStringProperty", 3, stringAllowedValues.size());
		assertTrue("String 1 not in allowed values", stringAllowedValues.contains("String 1"));
		assertTrue("String 2 not in allowed values", stringAllowedValues.contains("String 2"));
		assertTrue("String 3 not in allowed values", stringAllowedValues.contains("String 3"));
		
		assertEquals("Default values should be \"String 1\"", "String 1", allowedValuesStringProperty.getDefaultValueObject());

		Property allowedValuesIntProperty = s.getProperty(new URI("http://example.com/shapes/allowedValuesIntProperty"));
		assertNotNull("http://example.com/shapes/allowedValuesIntProperty property not in shape", s);

		Collection<?> intAllowedValues = allowedValuesIntProperty.getAllowedValuesCollection();
		assertEquals("Wrong number of allowedValues for allowedValuesIntProperty", 2, intAllowedValues.size());
		assertTrue("27 not in allowed values", intAllowedValues.contains(new Integer(27)));
		assertTrue("32 not in allowed values", intAllowedValues.contains(new Integer(32)));

		assertEquals("Default values should be 27", new Integer(27), allowedValuesIntProperty.getDefaultValueObject());
	}

	@SuppressWarnings("deprecation")
	@Test
	public void allowedValuesResourceTest() throws Exception {
		OslcTurtleProvider provider = new OslcTurtleProvider();
		InputStream is = ServiceProviderTest.class.getResourceAsStream("/allowedValuesResource.ttl");
		assertNotNull("Could not read file: /allowedValuesResource.ttl", is);
		
		// Make sure the content is properly interpreted as Turtle if the media type is "text/turtle;charset=UTF-8"
		@SuppressWarnings({ "unchecked", "rawtypes" })
		AllowedValues allowedValues = (AllowedValues) provider.readFrom((Class) AllowedValues.class,
				null,
				AllowedValues.class.getAnnotations(),
				MediaType.valueOf("text/turtle;charset=UTF-8"),
				null,
				is);
		assertNotNull("Allowed values is null", allowedValues);
		
		Collection<?> uriAllowedValues = allowedValues.getValues();
		assertEquals("Wrong number of allowedValues for allowedValuesUriProperty property", 2, uriAllowedValues.size());
		assertTrue("uri1 not in allowed values", uriAllowedValues.contains(new URI("http://example.com/ns#uri1")));
		assertTrue("uri2 not in allowed values", uriAllowedValues.contains(new URI("http://example.com/ns#uri2")));

		assertEquals("Old getAllowedValues() method should return 0 results since it only looks for strings", 0, allowedValues.getAllowedValues().length);
	}


	@SuppressWarnings("deprecation")
	@Test
	public void testAllowedValuesBackwardsCompatibility() throws Exception {
		ResourceShape s = readResourceShape("/allowedValues.ttl");
		assertNotNull("Shape is null", s);
		
		Property allowedValuesUriProperty = s.getProperty(new URI("http://example.com/shapes/allowedValuesUriProperty"));
		assertNotNull("http://example.com/shapes/allowedValuesUriProperty property not in shape", allowedValuesUriProperty);
		
		assertEquals("Old getAllowedValues() method should return 0 results since it only looks for strings", 0, allowedValuesUriProperty.getAllowedValues().length);
		assertNull("Old getDefaultValue() method should return null if default value is not a string", allowedValuesUriProperty.getDefaultValue());

		Property allowedValuesStringProperty = s.getProperty(new URI("http://example.com/shapes/allowedValuesStringProperty"));
		assertNotNull("http://example.com/shapes/allowedValuesStringProperty property not in shape", allowedValuesStringProperty);

		String[] stringAllowedValues = allowedValuesStringProperty.getAllowedValues();
		assertEquals("Wrong number of allowedValues for allowedValuesStringProperty", 3, stringAllowedValues.length);
		assertEquals("String 1", allowedValuesStringProperty.getDefaultValue());

		List<String> stringAllowedValuesList = Arrays.asList(stringAllowedValues);
		assertTrue("String 1 not in allowed values", stringAllowedValuesList.contains("String 1"));
		assertTrue("String 2 not in allowed values", stringAllowedValuesList.contains("String 2"));
		assertTrue("String 3 not in allowed values", stringAllowedValuesList.contains("String 3"));

		Property allowedValuesIntProperty = s.getProperty(new URI("http://example.com/shapes/allowedValuesIntProperty"));
		assertNotNull("http://example.com/shapes/allowedValuesIntProperty property not in shape", allowedValuesIntProperty);

		assertEquals("Old getAllowedValues() method should return 0 results since it only looks for strings", 0, allowedValuesIntProperty.getAllowedValues().length);
		assertNull("Old getDefaultValue() method should return null if default value is not a string", allowedValuesIntProperty.getDefaultValue());
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private ResourceShape readResourceShape(String turtleFile) throws IOException {
		OslcTurtleProvider provider = new OslcTurtleProvider();
		InputStream is = ServiceProviderTest.class.getResourceAsStream(turtleFile);
		assertNotNull("Could not read file: " + turtleFile, is);
		
		// Make sure the content is properly interpreted as Turtle if the media type is "text/turtle;charset=UTF-8"
		ResourceShape s = (ResourceShape) provider.readFrom((Class) ResourceShape.class,
				null,
				ResourceShape.class.getAnnotations(),
				MediaType.valueOf("text/turtle;charset=UTF-8"),
				null,
				is);
		assertNotNull("Shape was not read", s);

		return s;
	}
}
