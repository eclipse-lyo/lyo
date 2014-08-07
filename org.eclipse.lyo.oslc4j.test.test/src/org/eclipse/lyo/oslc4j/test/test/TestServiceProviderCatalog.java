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

import java.util.Set;

import junit.framework.TestCase;

import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryClient;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.Dialog;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;

public class TestServiceProviderCatalog
	   extends TestCase
{
	private static final Set<Class<?>> PROVIDERS = JenaProvidersRegistry.getProviders();

	private ServiceProviderRegistryClient serviceProviderRegistryClient;

	public TestServiceProviderCatalog()
	{
		super();
	}

	@Override
	protected void setUp()
	{
		serviceProviderRegistryClient = new ServiceProviderRegistryClient(PROVIDERS);
	}

	public void testServiceProviderCatalog()
	{
		assertNotNull(serviceProviderRegistryClient.getServiceProviderCatalog());
	}

	public void testServiceProviders()
	{
		final ServiceProvider[] serviceProviders = serviceProviderRegistryClient.getServiceProviders();

		assertNotNull(serviceProviders);
		assertTrue(serviceProviders.length > 0);

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			assertNotNull(serviceProvider.getAbout());
		}
	}

	public void testServices()
	{
		final ServiceProvider[] serviceProviders = serviceProviderRegistryClient.getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			assertNotNull(services);
			assertTrue(services.length > 0);
		}
	}

	public void testCreationDialogs()
	{
		final ServiceProvider[] serviceProviders = serviceProviderRegistryClient.getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				final Dialog[] dialogs = service.getCreationDialogs();

				assertNotNull(dialogs);

				for (final Dialog dialog : dialogs)
				{
					assertNotNull(dialog.getDialog());
					assertNotNull(dialog.getTitle());
				}
			}
		}
	}

	public void testCreationFactories()
	{
		final ServiceProvider[] serviceProviders = serviceProviderRegistryClient.getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				final CreationFactory[] creationFactories = service.getCreationFactories();

				assertNotNull(creationFactories);

				for (final CreationFactory creationFactory : creationFactories)
				{
					assertNotNull(creationFactory.getCreation());
					assertNotNull(creationFactory.getTitle());
				}
			}
		}
	}

	public void testQueryCapabilities()
	{
		final ServiceProvider[] serviceProviders = serviceProviderRegistryClient.getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				final QueryCapability[] queryCapabilities = service.getQueryCapabilities();

				assertNotNull(queryCapabilities);

				for (final QueryCapability queryCapability : queryCapabilities)
				{
					assertNotNull(queryCapability.getQueryBase());
					assertNotNull(queryCapability.getTitle());
				}
			}
		}
	}

	public void testSelectionDialogs()
	{
		final ServiceProvider[] serviceProviders = serviceProviderRegistryClient.getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				final Dialog[] dialogs = service.getSelectionDialogs();

				assertNotNull(dialogs);

				for (final Dialog dialog : dialogs)
				{
					assertNotNull(dialog.getDialog());
					assertNotNull(dialog.getTitle());
				}
			}
		}
	}
}