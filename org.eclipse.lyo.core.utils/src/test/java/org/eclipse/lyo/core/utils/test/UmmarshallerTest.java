/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 * 
 * Contributors:
 * 
 *    Samuel Padgett - Initial implementation
 *******************************************************************************/
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
