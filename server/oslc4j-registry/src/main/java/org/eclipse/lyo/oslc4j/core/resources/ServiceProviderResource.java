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
package org.eclipse.lyo.oslc4j.core.resources;

import java.net.URISyntaxException;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;

import org.eclipse.lyo.oslc4j.core.annotation.OslcCreationFactory;
import org.eclipse.lyo.oslc4j.core.annotation.OslcQueryCapability;
import org.eclipse.lyo.oslc4j.core.annotation.OslcService;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.servlet.ServiceProviderCatalogSingleton;

@OslcService(OslcConstants.OSLC_CORE_DOMAIN)
@Path("serviceProviders")
public class ServiceProviderResource
{
	@OslcQueryCapability
	(
		 title = "Service Provider Query Capability",
		 label = "Service Provider Query",
		 resourceShape = OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_SERVICE_PROVIDER,
		 resourceTypes = {OslcConstants.TYPE_SERVICE_PROVIDER},
		 usages = {OslcConstants.OSLC_USAGE_DEFAULT}
	)
	@GET
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public ServiceProvider[] getServiceProviders()
	{
		return ServiceProviderCatalogSingleton.getServiceProviders();
	}

	@GET
	@Path("{serviceProviderId}")
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public ServiceProvider getServiceProvider(@PathParam("serviceProviderId") final String serviceProviderId)
	{
		return ServiceProviderCatalogSingleton.getServiceProvider(serviceProviderId);
	}

	@GET
	@Path("{serviceProviderId}")
	@Produces({OslcMediaType.APPLICATION_X_OSLC_COMPACT_XML, OslcMediaType.APPLICATION_X_OSLC_COMPACT_JSON})
	public Compact getCompact(@PathParam("serviceProviderId") final String serviceProviderId)
	{
		final ServiceProvider serviceProvider = ServiceProviderCatalogSingleton.getServiceProvider(serviceProviderId);

		final Compact compact = new Compact();

		compact.setAbout(serviceProvider.getAbout());
		compact.setShortTitle(serviceProvider.getTitle());
		compact.setTitle(serviceProvider.getTitle());

		// TODO - Need icon for ServiceProvider compact.

		return compact;
	}

	@OslcCreationFactory
	(
		 title = "Service Provider Creation Factory",
		 label = "Service Provider Creation",
		 resourceShapes = {OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_SERVICE_PROVIDER},
		 resourceTypes = {OslcConstants.TYPE_SERVICE_PROVIDER},
		 usages = {OslcConstants.OSLC_USAGE_DEFAULT}
	)
	@POST
	@Consumes({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	@Produces({OslcMediaType.APPLICATION_RDF_XML, OslcMediaType.APPLICATION_XML, OslcMediaType.TEXT_XML, OslcMediaType.APPLICATION_JSON, OslcMediaType.TEXT_TURTLE})
	public Response registerServiceProvider(@Context final HttpServletRequest httpServletRequest,
													 final ServiceProvider	  serviceProvider)
		   throws URISyntaxException
	{
		final ServiceProvider registeredServiceProvider = ServiceProviderCatalogSingleton.registerServiceProvider(httpServletRequest,
																												  serviceProvider);

		return Response.created(registeredServiceProvider.getAbout()).entity(registeredServiceProvider).build();
	}

	@DELETE
	@Path("{serviceProviderId}")
	public Response deregisterServiceProvider(@PathParam("serviceProviderId") final String serviceProviderId)
	{
		ServiceProviderCatalogSingleton.deregisterServiceProvider(serviceProviderId);

		return Response.ok().build();
	}
}
