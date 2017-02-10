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
 *	  Michael Fiedler	   - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.test.test;

import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

public class TestTypesTextXml extends TestTypesBase {
	public TestTypesTextXml()
	{
		super();
	}

	public void testResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcMediaType.TEXT_XML);
	}

	public void testCreate()
	{
		testCreate(OslcMediaType.TEXT_XML);
	}

	public void testRetrieve()
	{
		testRetrieve(OslcMediaType.TEXT_XML);
	}

	public void testRetrieves()
	{
		testRetrieves(OslcMediaType.TEXT_XML);
	}

	public void testRetrieveCollection()
	{
		testRetrieveCollection(OslcMediaType.TEXT_XML);
	}

	public void testRetrieveError()
	{
		testRetrieveError(OslcMediaType.TEXT_XML);
	}

	public void testRetrieveMessageBodyWriterError()
	{
		testRetrieveMessageBodyWriterError(OslcMediaType.TEXT_XML);
	}

	public void testRetrieveCompactMessageBodyWriterError()
	{
		testRetrieveCompactMessageBodyWriterError(OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML,
												  OslcMediaType.TEXT_XML);
	}

	public void testUpdate()
	{
		testUpdate(OslcMediaType.TEXT_XML);
	}

	public void testDelete()
	{
		testDelete(OslcMediaType.TEXT_XML);
	}

}
