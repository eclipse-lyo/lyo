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
package org.eclipse.lyo.oslc4j.application;

import java.net.URISyntaxException;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import org.apache.wink.common.AbstractDynamicResource;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;

/**
 * This class provides a generic JAX-RS resource to expose ResourceShapes for an OSLC Domain.  It is used internally
 * by {@link OslcWinkApplication}.
 */
public class OslcResourceShapeResource
	   extends AbstractDynamicResource
{
	private static final String BASE_URI = "http://localhost/validatingResourceShapes";

	private final String				resourceShapesPath;
	private final Map<String, Class<?>> resourcePathToResourceClassMap;

	public OslcResourceShapeResource(final String				 resourceShapesPath,
									 final Map<String, Class<?>> resourcePathToResourceClassMap)
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		super();

		this.resourceShapesPath				= resourceShapesPath;
		this.resourcePathToResourceClassMap = resourcePathToResourceClassMap;

		setPath(resourceShapesPath);

		// Verify each of the resource shapes provided is valid
		for (final Map.Entry<String, Class<?>> entry : resourcePathToResourceClassMap.entrySet())
		{
			ResourceShapeFactory.createResourceShape(BASE_URI,
													 resourceShapesPath,
													 entry.getKey(),
													 entry.getValue());
		}
	}

	@GET
	@Path("{resourceShapePath}")
	@Produces(
			{OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType
					.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public ResourceShape getResourceShape(@Context final HttpServletRequest httpServletRequest,
			@PathParam("resourceShapePath") final String resourceShapePath)
			throws OslcCoreApplicationException, URISyntaxException {
		final Class<?> resourceClass = resourcePathToResourceClassMap.get(resourceShapePath);

		if (resourceClass != null) {
			final String servletUri = OSLC4JUtils.resolveServletUri(httpServletRequest);
			return ResourceShapeFactory.createResourceShape(servletUri, resourceShapesPath,
					resourceShapePath, resourceClass);
		}

		throw new WebApplicationException(Response.Status.NOT_FOUND);
	}
}
