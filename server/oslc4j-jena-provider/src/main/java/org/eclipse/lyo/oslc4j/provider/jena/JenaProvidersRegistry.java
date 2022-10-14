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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.oslc4j.provider.jena;

import java.util.HashSet;
import java.util.Set;

public final class JenaProvidersRegistry
{
	private static Set<Class<?>> PROVIDERS = new HashSet<>();

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
		return new HashSet<>(PROVIDERS);
	}

	public static final Set<Class<?>> setProviders (Set<Class<?>> providers)
	{
		if (providers != null && PROVIDERS != null)
		{
			PROVIDERS.clear();
			PROVIDERS.addAll(providers);
		}
		return new HashSet<>(PROVIDERS);
	}

	public static final Set<Class<?>> removeProviders (Set<Class<?>> providers)
	{
		if (providers != null && PROVIDERS != null)
		{
			PROVIDERS.removeAll(providers);
		}
		return new HashSet<>(PROVIDERS);
	}
}
