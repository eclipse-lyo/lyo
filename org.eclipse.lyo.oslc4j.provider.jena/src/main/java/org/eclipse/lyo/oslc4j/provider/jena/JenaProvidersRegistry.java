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
package org.eclipse.lyo.oslc4j.provider.jena;

import java.util.HashSet;
import java.util.Set;

public final class JenaProvidersRegistry
{
	private static Set<Class<?>> PROVIDERS = new HashSet<Class<?>>();

	static
	{
		PROVIDERS.add(OslcCompactRdfProvider.class);
		PROVIDERS.add(OslcRdfXmlArrayProvider.class);
		PROVIDERS.add(OslcRdfXmlCollectionProvider.class);
		PROVIDERS.add(OslcRdfXmlProvider.class);
		PROVIDERS.add(OslcXmlArrayProvider.class);
		PROVIDERS.add(OslcXmlCollectionProvider.class);
		PROVIDERS.add(OslcXmlProvider.class);
		PROVIDERS.add(OslcTurtleArrayProvider.class);
		PROVIDERS.add(OslcTurtleCollectionProvider.class);
		PROVIDERS.add(OslcTurtleProvider.class);
		PROVIDERS.add(OslcJsonLdProvider.class);
		PROVIDERS.add(OslcJsonLdArrayProvider.class);
		PROVIDERS.add(OslcJsonLdCollectionProvider.class);

	}

	private JenaProvidersRegistry()
	{
		super();
	}

	/**
	 * Returns a mutable set of provider classes.  Each request returns a new Set.
	 */
	public static final Set<Class<?>> getProviders()
	{
		return new HashSet<Class<?>>(PROVIDERS);
	}

	public static final Set<Class<?>> setProviders (Set<Class<?>> providers)
	{
		if (providers != null && PROVIDERS != null)
		{
			PROVIDERS.clear();
			PROVIDERS.addAll(providers);
		}
		return new HashSet<Class<?>>(PROVIDERS);
	}

	public static final Set<Class<?>> removeProviders (Set<Class<?>> providers)
	{
		if (providers != null && PROVIDERS != null)
		{
			PROVIDERS.removeAll(providers);
		}
		return new HashSet<Class<?>>(PROVIDERS);
	}
}
