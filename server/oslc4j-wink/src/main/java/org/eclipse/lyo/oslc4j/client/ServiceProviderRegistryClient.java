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
package org.eclipse.lyo.oslc4j.client;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.HttpHeaders;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreDeregistrationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreRegistrationException;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.core.model.ServiceProviderCatalog;

/**
 * This class provides methods to {@link #registerServiceProvider(ServiceProvider) register} and
 * {@link #deregisterServiceProvider(URI) deregister} {@link ServiceProvider}'s.
 *
 * This class also provides a method to {@link #getServiceProviders() retrieve} the registered {@link ServiceProvider}'s.
 */
public final class ServiceProviderRegistryClient
{
	private final OslcRestClient client;

	public ServiceProviderRegistryClient(final Set<Class<?>> providers,
			 final String		 mediaType,
			 final String		 uri) 
	{
		super();

		this.client = new OslcRestClient(providers,
										 uri,
										 mediaType);
	}

	public ServiceProviderRegistryClient(final Set<Class<?>> providers,
										 final String		 mediaType)
	{
		this(providers,				  
			 mediaType, 
			 ServiceProviderRegistryURIs.getServiceProviderRegistryURI());
	}

	/**
	 * Construct a client to assist with registering and deregistering {@link ServiceProvider}'s.
	 */
	public ServiceProviderRegistryClient(final Set<Class<?>> providers)
	{
		this(providers,
			 OslcMediaType.APPLICATION_RDF_XML);
	}

	/**
	 * Register a {@link ServiceProvider}.
	 */
	public URI registerServiceProvider(final ServiceProvider serviceProviderToRegister)
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		final URI typeServiceProviderURI = new URI(OslcConstants.TYPE_SERVICE_PROVIDER);
		final URI oslcUsageDefault		 = new URI(OslcConstants.OSLC_USAGE_DEFAULT);

		final ServiceProvider[] serviceProviders;

		// We have to first get the ServiceProvider for ServiceProviders and then find the CreationFactory for a ServiceProvider

		// We first try for a ServiceProviderCatalog
		final ServiceProviderCatalog serviceProviderCatalog = getServiceProviderCatalog();

		if (serviceProviderCatalog != null)
		{
			serviceProviders = serviceProviderCatalog.getServiceProviders();
		}
		else
		{
			// Secondly we try for a ServiceProvider which is acting as a ServiceProvider registry
			final ServiceProvider serviceProvider = getServiceProvider();

			if (serviceProvider != null)
			{
				serviceProviders = new ServiceProvider[] {serviceProvider};
			}
			else
			{
				throw new OslcCoreRegistrationException(serviceProviderToRegister,
														HttpServletResponse.SC_NOT_FOUND,
														"ServiceProviderCatalog");
			}
		}

		if (serviceProviders != null)
		{
			CreationFactory firstCreationFactory		= null;
			CreationFactory firstDefaultCreationFactory = null;

			for (int serviceProviderIndex = 0;
				 ((serviceProviderIndex < serviceProviders.length) &&
				  (firstDefaultCreationFactory == null));
				 serviceProviderIndex++)
			{
				final ServiceProvider serviceProvider = serviceProviders[serviceProviderIndex];

				final Service[] services = serviceProvider.getServices();

				if (services != null)
				{
					for (int serviceIndex = 0;
						 ((serviceIndex < services.length) &&
						  (firstDefaultCreationFactory == null));
						 serviceIndex++)
					{
						final Service service = services[serviceIndex];

						final CreationFactory[] creationFactories = service.getCreationFactories();

						if (creationFactories != null)
						{
							for (int creationFactoryIndex = 0;
								 ((creationFactoryIndex < creationFactories.length) &&
								  (firstDefaultCreationFactory == null));
								 creationFactoryIndex++)
							{
								final CreationFactory creationFactory = creationFactories[creationFactoryIndex];

								final URI[] resourceTypes = creationFactory.getResourceTypes();

								if (resourceTypes != null)
								{
									for (int resourceTypeIndex = 0;
										 ((resourceTypeIndex < resourceTypes.length) &&
										  (firstDefaultCreationFactory == null));
										 resourceTypeIndex++)
									{
										final URI resourceType = resourceTypes[resourceTypeIndex];

										if (typeServiceProviderURI.equals(resourceType))
										{
											if (firstCreationFactory == null)
											{
												firstCreationFactory = creationFactory;
											}

											final URI[] usages = creationFactory.getUsages();

											for (int usageIndex = 0;
												 ((usageIndex < usages.length) &&
												  (firstDefaultCreationFactory == null));
												 usageIndex++)
											{
												final URI usage = usages[usageIndex];

												if (oslcUsageDefault.equals(usage))
												{
													firstDefaultCreationFactory = creationFactory;
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}

			if (firstCreationFactory != null)
			{
				final CreationFactory creationFactory = firstDefaultCreationFactory != null ? firstDefaultCreationFactory : firstCreationFactory;

				final URI creation = creationFactory.getCreation();

				final OslcRestClient oslcRestClient = new OslcRestClient(client.getProviders(),
																		 creation);

				final ClientResponse clientResponse = oslcRestClient.addOslcResourceReturnClientResponse(serviceProviderToRegister);

				final int statusCode = clientResponse.getStatusCode();

				if (statusCode != HttpServletResponse.SC_CREATED)
				{
					throw new OslcCoreRegistrationException(serviceProviderToRegister,
															statusCode,
															clientResponse.getMessage());
				}

				final String location = clientResponse.getHeaders().getFirst(HttpHeaders.LOCATION);

				return new URI(location);
			}
		}

		throw new OslcCoreRegistrationException(serviceProviderToRegister,
												HttpServletResponse.SC_NOT_FOUND,
												"CreationFactory");
	}

	/**
	 * Remove registration for a {@link ServiceProvider}.
	 */
	public void deregisterServiceProvider(final URI serviceProviderURI)
		   throws OslcCoreApplicationException
	{
		final ClientResponse clientResponse = client.getClientResource().uri(serviceProviderURI).delete();

		final int statusCode = clientResponse.getStatusCode();
		if (statusCode != HttpServletResponse.SC_OK)
		{
			throw new OslcCoreDeregistrationException(serviceProviderURI,
													  statusCode,
													  clientResponse.getMessage());
		}
	}

	/**
	 * If a {@link ServiceProviderCatalog} is being used, this will return that object.
	 * Otherwise null will be returned.
	 */
	public ServiceProviderCatalog getServiceProviderCatalog()
	{
		return client.getOslcResource(ServiceProviderCatalog.class);
	}

	/**
	 * If a {@link ServiceProvider} is being used as a {@link ServiceProvider} registry without an owning {@link ServiceProviderCatalog},
	 * this will return the {@link ServiceProvider}.
	 * Otherwise null will be returned.
	 */
	public ServiceProvider getServiceProvider()
	{
		return client.getOslcResource(ServiceProvider.class);
	}

	/**
	 * Return the registered {@link ServiceProvider}'s.
	 */
	public ServiceProvider[] getServiceProviders()
	{
		// We first try for a ServiceProviderCatalog
		final ServiceProviderCatalog serviceProviderCatalog = getServiceProviderCatalog();

		if (serviceProviderCatalog != null)
		{
			return serviceProviderCatalog.getServiceProviders();
		}

		// Secondly we try for a ServiceProvider which is acting as a ServiceProvider registry
		final ServiceProvider serviceProvider = getServiceProvider();

		if (serviceProvider != null)
		{
			final Service[] services = serviceProvider.getServices();

			if (services != null)
			{
				QueryCapability firstQueryCapability		= null;
				QueryCapability firstDefaultQueryCapability = null;

				for (int serviceIndex = 0;
					 ((serviceIndex < services.length) &&
					  (firstDefaultQueryCapability == null));
					 serviceIndex++)
				{
					final Service service = services[serviceIndex];

					final QueryCapability[] queryCapabilities = service.getQueryCapabilities();

					if (queryCapabilities != null)
					{
						for (int queryCapabilityIndex = 0;
							 ((queryCapabilityIndex < queryCapabilities.length) &&
							  (firstDefaultQueryCapability == null));
							 queryCapabilityIndex++)
						{
							final QueryCapability queryCapability = queryCapabilities[queryCapabilityIndex];

							final URI[] resourceTypes = queryCapability.getResourceTypes();

							if (resourceTypes != null)
							{
								for (int resourceTypeIndex = 0;
									 ((resourceTypeIndex < resourceTypes.length) &&
									  (firstDefaultQueryCapability == null));
									 resourceTypeIndex++)
								{
									final URI resourceType = resourceTypes[resourceTypeIndex];

									if (OslcConstants.TYPE_SERVICE_PROVIDER.equals(String.valueOf(resourceType)))
									{
										if (firstQueryCapability == null)
										{
											firstQueryCapability = queryCapability;
										}

										final URI[] usages = queryCapability.getUsages();

										for (int usageIndex = 0;
											 ((usageIndex < usages.length) &&
											  (firstDefaultQueryCapability == null));
											 usageIndex++)
										{
											final URI usage = usages[usageIndex];

											if (OslcConstants.OSLC_USAGE_DEFAULT.equals(String.valueOf(usage)))
											{
												firstDefaultQueryCapability = queryCapability;
											}
										}
									}
								}
							}
						}
					}
				}

				if (firstQueryCapability != null)
				{
					final QueryCapability queryCapability = firstDefaultQueryCapability != null ? firstDefaultQueryCapability : firstQueryCapability;

					final URI queryBase = queryCapability.getQueryBase();

					// Foundation Registry Services requires the query string of oslc.select=* in order to flesh out the ServiceProviders
					final String query = queryBase.toString() + "?oslc.select=*";

					final OslcRestClient oslcRestClient = new OslcRestClient(client.getProviders(),
																			 query);

					return oslcRestClient.getOslcResources(ServiceProvider[].class);
				}
			}
		}

		return null;
	}

	public OslcRestClient getClient() {
		return client;
	}
}
