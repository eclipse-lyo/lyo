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

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;

import junit.framework.TestCase;

import org.eclipse.lyo.oslc4j.client.OslcRestClient;
import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryClient;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.provider.json4j.Json4JProvidersRegistry;
import org.eclipse.lyo.oslc4j.test.Constants;

public abstract class TestBase
	   extends TestCase
{
	protected static final Set<Class<?>> PROVIDERS = new HashSet<Class<?>>();

	static
	{
		PROVIDERS.addAll(JenaProvidersRegistry.getProviders());
		PROVIDERS.addAll(Json4JProvidersRegistry.getProviders());
	}

	protected TestBase()
	{
		super();
	}

	protected static String getCreation(final String mediaType,
										final String domain,
										final String type)
	{
		final ServiceProvider[] serviceProviders = new ServiceProviderRegistryClient(PROVIDERS, mediaType).getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				if (domain.equals(String.valueOf(service.getDomain())))
				{
					final CreationFactory[] creationFactories = service.getCreationFactories();

					for (final CreationFactory creationFactory : creationFactories)
					{
						final URI[] resourceTypes = creationFactory.getResourceTypes();

						for (final URI resourceType : resourceTypes)
						{
							if (resourceType.toString().equals(type))
							{
								return creationFactory.getCreation().toString();
							}
						}
					}
				}
			}
		}

		fail("Unable to retrieve creation for type '" + type + "'");

		return null;
	}

	protected static String getQueryBase(final String mediaType,
										 final String desiredUsage,
										 final String type)
	{
		final ServiceProvider[] serviceProviders = new ServiceProviderRegistryClient(PROVIDERS, mediaType).getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				if (Constants.TEST_DOMAIN.equals(String.valueOf(service.getDomain())))
				{
					final QueryCapability[] queryCapabilities = service.getQueryCapabilities();

					for (final QueryCapability queryCapability : queryCapabilities)
					{
						final URI[] resourceTypes = queryCapability.getResourceTypes();
						final URI[] usages		  = queryCapability.getUsages();

						boolean usageFound = false;

						for (final URI usage : usages)
						{
							if (usage.toString().equals(desiredUsage))
							{
								usageFound = true;

								break;
							}
						}

						if (usageFound)
						{
							for (final URI resourceType : resourceTypes)
							{
								if (resourceType.toString().equals(type))
								{
									return queryCapability.getQueryBase().toString();
								}
							}
						}
					}
				}
			}
		}

		fail("Unable to retrieve queryBase for type '" + type + "'");

		return null;
	}

	protected static ResourceShape getResourceShape(final String mediaType,
													final String type)
	{
		final ServiceProvider[] serviceProviders = new ServiceProviderRegistryClient(PROVIDERS, mediaType).getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				if (Constants.TEST_DOMAIN.equals(String.valueOf(service.getDomain())))
				{
					final QueryCapability[] queryCapabilities = service.getQueryCapabilities();

					for (final QueryCapability queryCapability : queryCapabilities)
					{
						final URI[] resourceTypes = queryCapability.getResourceTypes();

						for (final URI resourceType : resourceTypes)
						{
							if (resourceType.toString().equals(type))
							{
								final URI resourceShape = queryCapability.getResourceShape();

								if (resourceShape != null)
								{
									final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																							 resourceShape,
																							 mediaType);

									return oslcRestClient.getOslcResource(ResourceShape.class);
								}
							}
						}
					}
				}
			}
		}

		fail("Unable to retrieve resource shape for type '" + type + "'");

		return null;
	}

	protected static void verifyResourceShape(final ResourceShape resourceShape,
											  final String		  type)
			  throws URISyntaxException
	{
		assertNotNull(resourceShape);

		final URI[] describes = resourceShape.getDescribes();
		assertNotNull(describes);
		assertTrue(describes.length > 0);

		if (type != null)
		{
			assertTrue(Arrays.asList(describes).contains(new URI(type)));
		}

		final org.eclipse.lyo.oslc4j.core.model.Property[] properties = resourceShape.getProperties();

		assertNotNull(properties);
		assertTrue(properties.length > 0);

		for (final org.eclipse.lyo.oslc4j.core.model.Property property : properties)
		{
			final String name				= property.getName();
			final URI	 propertyDefinition = property.getPropertyDefinition();

			assertNotNull(name);
			assertNotNull(property.getOccurs());
			assertNotNull(propertyDefinition);
			assertNotNull(property.getValueType());

			assertTrue("propertyDefinition [" + propertyDefinition.toString() + "], name [" + name + "]",
					   propertyDefinition.toString().endsWith(name));
		}
	}

	protected static void verifyWebApplicationException(final WebApplicationException	 webApplicationException,
														final Class<? extends Exception> expectedCause)
	{
		final Response response = webApplicationException.getResponse();
		assertNotNull(response);

		final int status = response.getStatus();
		assertEquals(Response.Status.BAD_REQUEST.getStatusCode(), status);

		final Throwable cause = webApplicationException.getCause();
		assertTrue(expectedCause.isInstance(cause));

		final Object entity = response.getEntity();
		assertTrue(entity instanceof Error);

		final Error error = (Error) entity;
		assertEquals(String.valueOf(status), error.getStatusCode());

		assertEquals(cause.getMessage(), error.getMessage());
	}
}