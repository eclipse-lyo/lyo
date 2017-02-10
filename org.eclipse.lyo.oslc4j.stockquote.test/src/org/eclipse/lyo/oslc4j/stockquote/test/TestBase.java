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
 *	   Samuel Padgett		- fix problems with test ordering
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.stockquote.test;

import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.oslc4j.client.OslcRestClient;
import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryClient;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.CreationFactory;
import org.eclipse.lyo.oslc4j.core.model.QueryCapability;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.Service;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.provider.json4j.Json4JProvidersRegistry;
import org.eclipse.lyo.oslc4j.stockquote.Constants;
import org.eclipse.lyo.oslc4j.stockquote.Exchange;
import org.eclipse.lyo.oslc4j.stockquote.StockQuote;

import static org.junit.Assert.*;

public abstract class TestBase
{
	private static final String EXCHANGE = Exchange.NASDAQ.toString();
	private static final String SYMBOL	 = "COKE";

	private static final Set<Class<?>> PROVIDERS = new HashSet<Class<?>>();

	static
	{
		PROVIDERS.addAll(JenaProvidersRegistry.getProviders());
		PROVIDERS.addAll(Json4JProvidersRegistry.getProviders());
	}

	private static URI CREATED_STOCK_QUOTE_URI;

	protected TestBase()
	{
		super();
	}

	private static String getCreation(final String mediaType,
									  final String type)
	{
		final ServiceProvider[] serviceProviders = new ServiceProviderRegistryClient(PROVIDERS, mediaType).getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				if (Constants.STOCK_QUOTE_DOMAIN.equals(String.valueOf(service.getDomain())))
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

	private static String getQueryBase(final String mediaType,
									   final String type)
	{
		final ServiceProvider[] serviceProviders = new ServiceProviderRegistryClient(PROVIDERS, mediaType).getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				if (Constants.STOCK_QUOTE_DOMAIN.equals(String.valueOf(service.getDomain())))
				{
					final QueryCapability[] queryCapabilities = service.getQueryCapabilities();

					for (final QueryCapability queryCapability : queryCapabilities)
					{
						final URI[] resourceTypes = queryCapability.getResourceTypes();

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

		fail("Unable to retrieve queryBase for type '" + type + "'");

		return null;
	}

	private static ResourceShape getResourceShape(final String mediaType,
												  final String type)
	{
		final ServiceProvider[] serviceProviders = new ServiceProviderRegistryClient(PROVIDERS, mediaType).getServiceProviders();

		for (final ServiceProvider serviceProvider : serviceProviders)
		{
			final Service[] services = serviceProvider.getServices();

			for (final Service service : services)
			{
				if (Constants.STOCK_QUOTE_DOMAIN.equals(String.valueOf(service.getDomain())))
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

	private static void verifyStockQuote(final String	  mediaType,
										 final StockQuote stockQuote,
										 final boolean	  recurse)
	{
		assertNotNull(stockQuote);

		final URI	 aboutURI			= stockQuote.getAbout();
		final String exchangeString		= stockQuote.getExchange();
		final String identifierString	= stockQuote.getIdentifier();
		final String symbolString		= stockQuote.getSymbol();
		final URI	 serviceProviderURI = stockQuote.getServiceProvider();

		assertNotNull(aboutURI);
		assertEquals(EXCHANGE, exchangeString);
		assertNotNull(identifierString);
		assertNotNull(serviceProviderURI);
		assertEquals(SYMBOL, symbolString);

		assertTrue(aboutURI.toString().endsWith(identifierString));

		if (recurse)
		{
			final OslcRestClient aboutOSLCRestClient = new OslcRestClient(PROVIDERS,
																		  aboutURI,
																		  mediaType);

			verifyStockQuote(mediaType,
							 aboutOSLCRestClient.getOslcResource(stockQuote.getClass()),
							 false);

			final OslcRestClient serviceProviderOSLCRestClient = new OslcRestClient(PROVIDERS,
																					serviceProviderURI,
																					mediaType);

			final ServiceProvider serviceProvider = serviceProviderOSLCRestClient.getOslcResource(ServiceProvider.class);

			assertNotNull(serviceProvider);
		}
	}

	private static void verifyCompact(final String	mediaType,
									  final Compact compact)
	{
		assertNotNull(compact);

		final URI	 aboutURI		  = compact.getAbout();
		final String shortTitleString = compact.getShortTitle();
		final String titleString	  = compact.getTitle();

		assertNotNull(aboutURI);
		assertNotNull(shortTitleString);
		assertNotNull(titleString);

		final OslcRestClient aboutOSLCRestClient = new OslcRestClient(PROVIDERS,
																	  aboutURI,
																	  mediaType);

		verifyStockQuote(mediaType,
						 aboutOSLCRestClient.getOslcResource(StockQuote.class),
						 false);
	}

	private static void verifyResourceShape(final ResourceShape resourceShape,
											final String		type)
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

			assertNotNull(property.getDescription());
			assertNotNull(name);
			assertNotNull(property.getOccurs());
			assertNotNull(propertyDefinition);
			assertNotNull(property.getTitle());
			assertNotNull(property.getValueType());

			assertTrue("propertyDefinition [" + propertyDefinition.toString() + "], name [" + name + "]",
					   propertyDefinition.toString().endsWith(name));
		}
	}

	protected void testResourceShape(final String mediaType)
			  throws URISyntaxException
	{
		final ResourceShape resourceShape = getResourceShape(mediaType,
															 Constants.TYPE_STOCK_QUOTE);

		verifyResourceShape(resourceShape,
							Constants.TYPE_STOCK_QUOTE);
	}

	protected void testCompact(final String compactMediaType,
							   final String normalMediaType)
	{
		assertNotNull(CREATED_STOCK_QUOTE_URI);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 CREATED_STOCK_QUOTE_URI,
																 compactMediaType);

		final Compact compact = oslcRestClient.getOslcResource(Compact.class);

		verifyCompact(normalMediaType,
					  compact);
	}

	protected void createTestRecord(final String mediaType)
	{
		CREATED_STOCK_QUOTE_URI = null;

		final StockQuote stockQuote = new StockQuote();

		stockQuote.setExchange(EXCHANGE);
		stockQuote.setSymbol(SYMBOL);

		final String creation = getCreation(mediaType,
											Constants.TYPE_STOCK_QUOTE);

		assertNotNull(creation);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 creation,
																 mediaType);

		final StockQuote addedStockQuote = oslcRestClient.addOslcResource(stockQuote);

		verifyStockQuote(mediaType,
						 addedStockQuote,
						 true);

		CREATED_STOCK_QUOTE_URI = addedStockQuote.getAbout();
	}

	protected void deleteTestRecord(final String mediaType)
	{
		assertNotNull(CREATED_STOCK_QUOTE_URI);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 CREATED_STOCK_QUOTE_URI,
																 mediaType);

		final ClientResponse clientResponse = oslcRestClient.removeOslcResourceReturnClientResponse();

		assertNotNull(clientResponse);
		assertEquals(HttpURLConnection.HTTP_NO_CONTENT, clientResponse.getStatusCode());

		assertNull(oslcRestClient.getOslcResource(StockQuote.class));

		CREATED_STOCK_QUOTE_URI = null;
	}

	protected void testRetrieve(final String mediaType)
	{
		assertNotNull(CREATED_STOCK_QUOTE_URI);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 CREATED_STOCK_QUOTE_URI,
																 mediaType);

		final StockQuote stockQuote = oslcRestClient.getOslcResource(StockQuote.class);

		verifyStockQuote(mediaType,
						 stockQuote,
						 true);
	}

	protected void testRetrieves(final String mediaType)
	{
		assertNotNull(CREATED_STOCK_QUOTE_URI);

		final String queryBase = getQueryBase(mediaType,
											  Constants.TYPE_STOCK_QUOTE);

		assertNotNull(queryBase);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 queryBase,
																 mediaType);

		final StockQuote[] stockQuotes = oslcRestClient.getOslcResources(StockQuote[].class);

		assertNotNull(stockQuotes);
		assertTrue(stockQuotes.length > 0);

		boolean found = false;

		for (final StockQuote stockQuote : stockQuotes)
		{
			if (CREATED_STOCK_QUOTE_URI.equals(stockQuote.getAbout()))
			{
				verifyStockQuote(mediaType,
								 stockQuote,
								 true);

				found = true;
			}
		}

		assertTrue(found);
	}
}