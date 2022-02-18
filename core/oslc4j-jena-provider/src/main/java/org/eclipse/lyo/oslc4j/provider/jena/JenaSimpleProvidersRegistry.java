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

public final class JenaSimpleProvidersRegistry
{
	private static final Set<Class<?>> PROVIDERS = new HashSet<>();

	static
	{
		PROVIDERS.add(OslcCompactRdfProvider.class);
		PROVIDERS.add(OslcSimpleRdfXmlArrayProvider.class);
		PROVIDERS.add(OslcSimpleRdfXmlCollectionProvider.class);
		PROVIDERS.add(OslcRdfXmlProvider.class);
	}

	private JenaSimpleProvidersRegistry()
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
}
