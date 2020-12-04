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
package org.eclipse.lyo.oslc4j.application;

import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.core.Application;

import org.apache.wink.common.WinkApplication;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;

/**
 * This class extends {@link WinkApplication} and supports a generic ResourceShape JAX-RS resource.
 */
public abstract class OslcWinkApplication
	   extends WinkApplication
{
	protected final Set<Object>	  instances = new HashSet<Object>();

	protected final Set<Class<?>> resourceClasses;

	/**
	 * @param resourceClasses The set of resource classes to be exposed via {@link Application#getClasses()}.
	 * @param resourceShapesPath The JAX-RS path to the resource shapes resource.
	 * @param resourcePathToResourceClassMap A mapping from a resource path to its related Java bean.
	 * @throws URISyntaxException
	 * @throws OslcCoreApplicationException
	 */
	public OslcWinkApplication(final Set<Class<?>>		   resourceClasses,
							   final String				   resourceShapesPath,
							   final Map<String, Class<?>> resourcePathToResourceClassMap)
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		super();

		final OslcResourceShapeResource oslcResourceShapeResource = new OslcResourceShapeResource(resourceShapesPath,
																								  resourcePathToResourceClassMap);

		instances.add(oslcResourceShapeResource);

		this.resourceClasses = resourceClasses;
	}
	
	//Bugzilla 392780
	// Called by OslcDynamicWinkApplication
	public OslcWinkApplication(final Set<Class<?>> resourceClasses,
			final String resourceShapesPath)
			throws OslcCoreApplicationException, URISyntaxException {
		super();

		this.resourceClasses = resourceClasses;
	}

	@Override
	public final Set<Class<?>> getClasses()
	{
		return resourceClasses;
	}

	@Override
	public final Set<Object> getInstances()
	{
		return instances;
	}

	@Override
	public final double getPriority()
	{
		return WinkApplication.DEFAULT_PRIORITY;
	}
}