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

import static org.junit.Assert.assertNotNull;

import java.io.InputStream;

import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.OslcTurtleProvider;
import org.junit.Test;

import jakarta.ws.rs.core.MediaType;

public class TurtleTest {
	@Test
	@SuppressWarnings({
		"unchecked",
		"rawtypes"
	})
	public void testContentTypeTurtleUTF8() throws Exception {
		OslcTurtleProvider provider = new OslcTurtleProvider();
		InputStream is = ServiceProviderTest.class.getResourceAsStream("/provider.ttl");
		assertNotNull("Could not read file: provider.ttl", is);
		
		// Make sure the content is properly interpreted as Turtle if the media type is "text/turtle;charset=UTF-8"
		ServiceProvider p = (ServiceProvider) provider.readFrom((Class) ServiceProvider.class,
				null,
				ServiceProvider.class.getAnnotations(),
				MediaType.valueOf("text/turtle;charset=UTF-8"),
				null,
				is);
		assertNotNull("Provider was not read", p);
	}
}
