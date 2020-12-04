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
