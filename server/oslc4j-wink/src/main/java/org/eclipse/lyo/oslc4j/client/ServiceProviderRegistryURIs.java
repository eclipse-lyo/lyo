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

import java.net.InetAddress;
import java.util.logging.Logger;

public final class ServiceProviderRegistryURIs
{
	private static final Logger LOGGER = Logger.getLogger(ServiceProviderRegistryURIs.class.getName());

	private static final String SYSTEM_PROPERTY_NAME_REGISTRY_URI = ServiceProviderRegistryURIs.class.getPackage().getName() + ".registryuri";
	private static final String SYSTEM_PROPERTY_NAME_UI_URI		  = ServiceProviderRegistryURIs.class.getPackage().getName() + ".uiuri";

	private static final String SERVICE_PROVIDER_REGISTRY_URI;
	private static final String UI_URI;

	static
	{
		final String registryURI = System.getProperty(SYSTEM_PROPERTY_NAME_REGISTRY_URI);
		final String uiURI		 = System.getProperty(SYSTEM_PROPERTY_NAME_UI_URI);

		String defaultBase = null;

		if ((registryURI == null) ||
			(uiURI == null))
		{
			// We need at least one default URI

			String hostName = "localhost";

			try
			{
				hostName = InetAddress.getLocalHost().getCanonicalHostName();
			}
			catch (final Exception exception)
			{
				// Default to localhost
			}

			defaultBase = "http://" + hostName + ":8080/";
		}

		if (registryURI != null)
		{
			SERVICE_PROVIDER_REGISTRY_URI = registryURI;
		}
		else
		{
			// In order to force Jena to show SPC first in XML, add a bogus identifier to the SPC URI.
			// This is because Jena can show an object anywhere in its graph where it is referenced.  Since the
			// SPC URI (without tailing identifier) is the same as its QueryCapability's queryBase, it can
			// be strangely rendered with the SPC nested under the queryBase.
			// This also allows us to distinguish between array and single results within the ServiceProviderCatalogResource.
			SERVICE_PROVIDER_REGISTRY_URI = defaultBase + "OSLC4JRegistry/catalog/singleton";

			LOGGER.warning("System property '" + SYSTEM_PROPERTY_NAME_REGISTRY_URI + "' not set.  Using calculated value '" + SERVICE_PROVIDER_REGISTRY_URI + "'");
		}

		if (uiURI != null)
		{
			UI_URI = uiURI;
		}
		else
		{
			UI_URI = defaultBase + "OSLC4JUI";

			LOGGER.warning("System property '" + SYSTEM_PROPERTY_NAME_UI_URI + "' not set.	Using calculated value '" + UI_URI + "'");
		}
	}

	private ServiceProviderRegistryURIs()
	{
		super();
	}

	public static String getServiceProviderRegistryURI()
	{
		return SERVICE_PROVIDER_REGISTRY_URI;
	}

	public static String getUIURI()
	{
		return UI_URI;
	}
}
