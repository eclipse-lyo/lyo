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

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.eclipse.lyo.oslc4j.core.annotation.OslcCreationFactory;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDialog;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.Preview;
import org.eclipse.lyo.oslc4j.test.Constants;
import org.eclipse.lyo.oslc4j.test.Persistence;
import org.eclipse.lyo.oslc4j.test.Test;

@OslcService(Constants.TEST_DOMAIN)
@Path("tests")
public class TestResource
{
	public TestResource()
	{
		super();
	}

	@OslcDialog
	(
		title = "Test Selection Dialog",
		label = "Test Selection Dialog",
		uri = "",
		hintWidth = "1000px",
		hintHeight = "600px",
		resourceTypes = {Constants.TYPE_TEST},
		usages = {OslcConstants.OSLC_USAGE_DEFAULT}
	)
	@OslcQueryCapability
	(
		title = "Test Query Capability",
		label = "Test Query",
		resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_TEST,
		resourceTypes = {Constants.TYPE_TEST},
		usages = {OslcConstants.OSLC_USAGE_DEFAULT}
	)
	@GET
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public Test[] getTests()
	{
		return Persistence.getTests();
	}

	@GET
	@Path("{testId}")
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public Test getTest(@PathParam("testId") final String testId)
	{
		final Test test = Persistence.getTest(testId);

		if (test != null)
		{
			return test;
		}

		throw new WebApplicationException(Status.NOT_FOUND);
	}

	@GET
	@Path("{testId}")
	@Produces({OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML, OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON})
	public Compact getCompact(@PathParam("testId") final String testId)
		   throws URISyntaxException
	{
		final Test test = Persistence.getTest(testId);

		if (test != null)
		{
			final Preview largePreview = new Preview();

			largePreview.setDocument(new URI("http://www.yourcompany.com/doc.jsp"));
			largePreview.setHintHeight("200px");
			largePreview.setHintWidth("300px");
			largePreview.setInitialHeight("100px");

			final Compact compact = new Compact();

			compact.setAbout(test.getAbout());
			compact.setIcon(new URI("http://www.yourcompany.com/icon.gif"));
			compact.setLargePreview(largePreview);
			compact.setShortTitle("shortTitle");
			compact.setTitle("title");

			return compact;
		}

		throw new WebApplicationException(Status.NOT_FOUND);
	}

	@OslcCreationFactory
	(
		 title = "Test Creation Factory",
		 label = "Test Creation",
		 resourceShapes = {OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_TEST},
		 resourceTypes = {Constants.TYPE_TEST},
		 usages = {OslcConstants.OSLC_USAGE_DEFAULT}
	)
	@POST
	@Consumes({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public Response addTest(@Context final HttpServletRequest httpServletRequest,
									 final Test				  test)
		   throws URISyntaxException
	{
		final long identifier = Persistence.getNextIdentifier();

		final URI about = new URI(httpServletRequest.getScheme(),
								  null,
								  httpServletRequest.getServerName(),
								  httpServletRequest.getServerPort(),
								  httpServletRequest.getContextPath() + "/tests/" + identifier,
								  null,
								  null);

		test.setAbout(about);
		test.setIdentifier(String.valueOf(identifier));

		Persistence.addTest(test);

		return Response.created(about).entity(test).build();
	}

	@PUT
	@Consumes({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	@Path("{testId}")
	public Response updateTest(@PathParam("testId") final String testId,
													final Test	 test)
	{
		final Test updatedTest = Persistence.updateTest(testId,
														test);

		if (updatedTest != null)
		{
			return Response.ok().build();
		}

		throw new WebApplicationException(Status.NOT_FOUND);
	}

	@DELETE
	@Path("{testId}")
	public Response deleteTest(@PathParam("testId") final String testId)
	{
		final Test test = Persistence.deleteTest(testId);

		if (test != null)
		{
			return Response.noContent().build();
		}

		throw new WebApplicationException(Status.NOT_FOUND);
	}
}
