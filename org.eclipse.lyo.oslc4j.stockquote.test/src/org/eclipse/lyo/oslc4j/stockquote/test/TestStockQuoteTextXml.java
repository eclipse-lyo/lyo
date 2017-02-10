/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *	   Michael Fiedler		 - initial API and implementation
 *	   Samuel Padgett		 - fix problems with test ordering
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.stockquote.test;

import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TestStockQuoteTextXml
	   extends TestBase
{
	public TestStockQuoteTextXml()
	{
		super();
	}

	@Before
	public void create()
	{
		createTestRecord(OslcMediaType.TEXT_XML);
	}

	@Test
	public void testResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcMediaType.TEXT_XML);
	}

	@Test
	public void testRetrieve()
	{
		testRetrieve(OslcMediaType.TEXT_XML);
	}

	@Test
	public void testRetrieves()
	{
		testRetrieves(OslcMediaType.TEXT_XML);
	}

	@After
	public void deleteTestRecord()
	{
		deleteTestRecord(OslcMediaType.TEXT_XML);
	}
}