/*******************************************************************************
 * Copyright (c) 2012, 2015 IBM Corporation.
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
package org.eclipse.lyo.oslc4j.test.resources;

import java.net.URI;
import java.net.URISyntaxException;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.test.Constants;
import org.eclipse.lyo.oslc4j.test.Test;

@OslcService(Constants.TEST_DOMAIN)
@Path("tests4")
public class TestMessageBodyWriterErrorResource
{
	public TestMessageBodyWriterErrorResource()
	{
		super();
	}

	@OslcQueryCapability
	(
		title = "Test MessageBodyWriter Error Query Capability",
		label = "Test MessageBodyWriter Error Query",
		resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_TEST,
		resourceTypes = {Constants.TYPE_TEST},
		usages = {Constants.USAGE_MESSAGE_BODY_WRITER_ERROR}
	)
	@GET
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public Test[] getTests()
		   throws URISyntaxException
	{
		final Test test = new Test();

		test.setUriProperty(new URI("relative"));

		return new Test[] {test};
	}

	@GET
	@Path("{testId}")
	@Produces({OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML, OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON})
	public Compact getCompact(@PathParam("testId") final String testId)
		   throws URISyntaxException
	{
		final Compact compact = new Compact();

		compact.setAbout(new URI("relative"));

		return compact;
	}
}
