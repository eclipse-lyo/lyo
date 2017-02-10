/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.test.test;

import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

public class TestTypesJson
	   extends TestTypesBase
{
	public TestTypesJson()
	{
		super();
	}

	@Override
	public void setUp()
	{
		testCreate(OslcMediaType.APPLICATION_JSON);
	}

	public void testResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcMediaType.APPLICATION_JSON);
	}

	public void testRetrieve()
	{
		testRetrieve(OslcMediaType.APPLICATION_JSON);
	}

	public void testRetrieveCompact()
	{
		testRetrieveCompact(OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON);
	}

	public void testRetrieves()
	{
		testRetrieves(OslcMediaType.APPLICATION_JSON);
	}

	public void testRetrieveCollection()
	{
		testRetrieveCollection(OslcMediaType.APPLICATION_JSON);
	}

	public void testRetrieveError()
	{
		testRetrieveError(OslcMediaType.APPLICATION_JSON);
	}

	public void testRetrieveMessageBodyWriterError()
	{
		testRetrieveMessageBodyWriterError(OslcMediaType.APPLICATION_JSON);
	}

	public void testRetrieveCompactMessageBodyWriterError()
	{
		testRetrieveCompactMessageBodyWriterError(OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON,
												  OslcMediaType.APPLICATION_JSON);
	}

	public void testUpdate()
	{
		testUpdate(OslcMediaType.APPLICATION_JSON);
	}

	@Override
	public void tearDown()
	{
		testDelete(OslcMediaType.APPLICATION_JSON);
	}
}