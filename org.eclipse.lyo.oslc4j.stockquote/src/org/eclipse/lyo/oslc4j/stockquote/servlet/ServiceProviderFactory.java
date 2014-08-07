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
package org.eclipse.lyo.oslc4j.stockquote.servlet;

import java.net.URI;
import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryURIs;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.PrefixDefinition;
import org.eclipse.lyo.oslc4j.core.model.Publisher;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.stockquote.Constants;
import org.eclipse.lyo.oslc4j.stockquote.resources.StockQuoteResource;

class ServiceProviderFactory
{
	private static Class<?>[] RESOURCE_CLASSES =
	{
		StockQuoteResource.class
	};

	private ServiceProviderFactory()
	{
		super();
	}

	public static ServiceProvider createServiceProvider(final String baseURI)
		   throws OslcCoreApplicationException, URISyntaxException
	{
		final ServiceProvider serviceProvider = org.eclipse.lyo.oslc4j.core.model.ServiceProviderFactory.createServiceProvider(baseURI,
																															   ServiceProviderRegistryURIs.getUIURI(),
																															   "OSLC Stock Quote Service Provider",
																															   "Reference Implementation OSLC Stock Quote Service Provider",
																															   new Publisher("Russell", "IBM"),
																															   RESOURCE_CLASSES
		);

		final PrefixDefinition[] prefixDefinitions =
		{
			new PrefixDefinition(OslcConstants.DCTERMS_NAMESPACE_PREFIX,   new URI(OslcConstants.DCTERMS_NAMESPACE)),
			new PrefixDefinition(OslcConstants.OSLC_CORE_NAMESPACE_PREFIX, new URI(OslcConstants.OSLC_CORE_NAMESPACE)),
			new PrefixDefinition(OslcConstants.RDF_NAMESPACE_PREFIX,	   new URI(OslcConstants.RDF_NAMESPACE)),
			new PrefixDefinition(OslcConstants.RDFS_NAMESPACE_PREFIX,	   new URI(OslcConstants.RDFS_NAMESPACE)),
			new PrefixDefinition(Constants.STOCK_QUOTE_NAMESPACE_PREFIX,   new URI(Constants.STOCK_QUOTE_NAMESPACE)),
		};

		serviceProvider.setPrefixDefinitions(prefixDefinitions);

		return serviceProvider;
	}
}
