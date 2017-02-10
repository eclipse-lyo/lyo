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
package org.eclipse.lyo.oslc4j.test.resources;

import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.lyo.oslc4j.application.OslcWinkApplication;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;
import org.eclipse.lyo.oslc4j.provider.json4j.Json4JProvidersRegistry;
import org.eclipse.lyo.oslc4j.test.Constants;
import org.eclipse.lyo.oslc4j.test.Nested;
import org.eclipse.lyo.oslc4j.test.Test;

public final class Oslc4JTestApplication
	   extends OslcWinkApplication
{
	private static final Set<Class<?>>		   RESOURCE_CLASSES							 = new HashSet<Class<?>>();
	private static final Map<String, Class<?>> RESOURCE_SHAPE_PATH_TO_RESOURCE_CLASS_MAP = new HashMap<String, Class<?>>();

	static
	{
		RESOURCE_CLASSES.addAll(JenaProvidersRegistry.getProviders());
		RESOURCE_CLASSES.addAll(Json4JProvidersRegistry.getProviders());
		RESOURCE_CLASSES.add(TestResource.class);
		RESOURCE_CLASSES.add(TestCollectionResource.class);
		RESOURCE_CLASSES.add(TestErrorResource.class);
		RESOURCE_CLASSES.add(TestMessageBodyWriterErrorResource.class);

		RESOURCE_SHAPE_PATH_TO_RESOURCE_CLASS_MAP.put(Constants.PATH_TEST,	 Test.class);
		RESOURCE_SHAPE_PATH_TO_RESOURCE_CLASS_MAP.put(Constants.PATH_NESTED, Nested.class);
	}

	public Oslc4JTestApplication()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		super(RESOURCE_CLASSES,
			  OslcConstants.PATH_RESOURCE_SHAPES,
			  RESOURCE_SHAPE_PATH_TO_RESOURCE_CLASS_MAP);
	}
}