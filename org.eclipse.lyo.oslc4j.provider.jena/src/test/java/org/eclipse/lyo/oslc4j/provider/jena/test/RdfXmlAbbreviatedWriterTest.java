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

import static org.junit.Assert.*;

import java.io.InputStream;

import org.eclipse.lyo.oslc4j.provider.jena.RdfXmlAbbreviatedWriter;
import org.junit.Test;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class RdfXmlAbbreviatedWriterTest {
	@Test
	public void testCircularReference()
	{
		InputStream is = RdfXmlAbbreviatedWriterTest.class.getResourceAsStream("/circular.xml");
		assertNotNull("Could not read file: circular.xml", is);
		Model m = ModelFactory.createDefaultModel();
		m.read(is, null);
		RdfXmlAbbreviatedWriter w = new RdfXmlAbbreviatedWriter();
		w.write(m, System.out, null);
	}
}
