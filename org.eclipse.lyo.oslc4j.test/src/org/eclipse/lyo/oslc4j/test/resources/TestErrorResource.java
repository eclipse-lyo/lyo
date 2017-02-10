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
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.ExtendedError;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.test.Constants;

@OslcService(Constants.TEST_DOMAIN)
@Path("tests3")
public class TestErrorResource
{
	public TestErrorResource()
	{
		super();
	}

	@OslcQueryCapability
	(
		title = "Test Error Query Capability",
		label = "Test Error Query",
		resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_TEST,
		resourceTypes = {Constants.TYPE_TEST},
		usages = {Constants.USAGE_ERROR}
	)
	@GET
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public Response getError()
		   throws URISyntaxException
	{
		final ExtendedError extendedError = new ExtendedError();

		extendedError.setHintHeight("100");
		extendedError.setHintWidth("200");
		extendedError.setMoreInfo(new URI("http://open-services.net/bin/view/Main/OslcCoreSpecification?sortcol=table;up=#Error_Responses"));
		extendedError.setRel("alternate");

		final Error error = new Error();

		error.setExtendedError(extendedError);
		error.setMessage("This is a bogus bad request status for testing purposes.");
		error.setStatusCode(String.valueOf(Response.Status.BAD_REQUEST.getStatusCode()));

		return Response.ok(error).build();
	}
}
