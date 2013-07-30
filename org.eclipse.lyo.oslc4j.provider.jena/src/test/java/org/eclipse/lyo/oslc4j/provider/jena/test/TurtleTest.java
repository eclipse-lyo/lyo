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
 *     Samuel Padgett - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena.test;

import static org.junit.Assert.assertNotNull;

import java.io.InputStream;

import javax.ws.rs.core.MediaType;

import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.OslcTurtleProvider;
import org.junit.Test;

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
