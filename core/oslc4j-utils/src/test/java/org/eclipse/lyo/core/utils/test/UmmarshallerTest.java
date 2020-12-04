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
package org.eclipse.lyo.core.utils.test;

import static org.junit.Assert.*;

import java.io.InputStream;

import org.eclipse.lyo.core.utils.marshallers.MarshallerConstants;
import org.eclipse.lyo.core.utils.marshallers.OSLC4JContext;
import org.eclipse.lyo.core.utils.marshallers.OSLC4JUnmarshaller;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.junit.Test;

public class UmmarshallerTest {
	@Test
	public void testReadRdfXml() throws Exception {
		InputStream is = UmmarshallerTest.class.getResourceAsStream("/provider.rdf");
		assertNotNull("Could not read file: provider.rdf", is);
		OSLC4JUnmarshaller um = OSLC4JContext.newInstance().createUnmarshaller();
		um.setMediaType(MarshallerConstants.MT_RDF_XML);
		ServiceProvider p = um.unmarshal(is, ServiceProvider.class);
		assertEquals("Blogging Service", p.getTitle());
	}
	
	@Test
	public void testReadTurtle() throws Exception {
		InputStream is = UmmarshallerTest.class.getResourceAsStream("/provider.ttl");
		assertNotNull("Could not read file: provider.ttl", is);
		OSLC4JUnmarshaller um = OSLC4JContext.newInstance().createUnmarshaller();
		um.setMediaType(MarshallerConstants.MT_TURTLE);
		ServiceProvider p = um.unmarshal(is, ServiceProvider.class);
		assertEquals("Blogging Service", p.getTitle());
	}
}
