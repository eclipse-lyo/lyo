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
 *	   Samuel Padgett - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.json4j.test;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.ws.rs.WebApplicationException;

import org.eclipse.lyo.oslc4j.provider.json4j.OslcRdfJsonProvider;
import org.eclipse.lyo.oslc4j.provider.json4j.test.resources.TestResource;
import org.junit.Test;

@SuppressWarnings("deprecation")
public class TestInvalidTypesJson
{
	@Test(expected = WebApplicationException.class)
	public void testInvalidJavaAboutRelativeURI()
		   throws IOException,
				  URISyntaxException
	{
		final TestResource relativeUri = new TestResource();

		relativeUri.setAbout(new URI("relative"));

		final OslcRdfJsonProvider oslcRdfJsonProvider = new OslcRdfJsonProvider();

		oslcRdfJsonProvider.writeTo(relativeUri,
									null,
									null,
									null,
									null,
									null,
									null);

	}
}
