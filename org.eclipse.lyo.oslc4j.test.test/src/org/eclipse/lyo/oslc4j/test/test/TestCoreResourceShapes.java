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
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.test.test;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Set;

import junit.framework.TestCase;

import org.eclipse.lyo.oslc4j.client.OslcRestClient;
import org.eclipse.lyo.oslc4j.client.ServiceProviderRegistryURIs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.Property;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;

public class TestCoreResourceShapes
	   extends TestCase
{
	private static final Set<Class<?>> PROVIDERS = JenaProvidersRegistry.getProviders();

	public void testCompactResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_COMPACT, OslcConstants.PATH_COMPACT);
	}

	public void testCreationFactoryResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_CREATION_FACTORY, OslcConstants.PATH_CREATION_FACTORY);
	}

	public void testDialogResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_DIALOG, OslcConstants.PATH_DIALOG);
	}

	public void testErrorResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_ERROR, OslcConstants.PATH_ERROR);
	}

	public void testExtendedErrorResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_EXTENDED_ERROR, OslcConstants.PATH_EXTENDED_ERROR);
	}

	public void testOAuthConfigurationResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_O_AUTH_CONFIGURATION, OslcConstants.PATH_OAUTH_CONFIGURATION);
	}

	public void testPrefixDefinitionResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_PREFIX_DEFINITION, OslcConstants.PATH_PREFIX_DEFINITION);
	}

	public void testPreviewResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_PREVIEW, OslcConstants.PATH_PREVIEW);
	}

	public void testPropertyResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_PROPERTY, OslcConstants.PATH_PROPERTY);
	}

	public void testPublisherResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_PUBLISHER, OslcConstants.PATH_PUBLISHER);
	}

	public void testQueryCapabilityResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_QUERY_CAPABILITY, OslcConstants.PATH_QUERY_CAPABILITY);
	}

	public void testResourceResourceResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_RESOURCE_SHAPE, OslcConstants.PATH_RESOURCE_SHAPE);
	}

	public void testServiceResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_SERVICE, OslcConstants.PATH_SERVICE);
	}

	public void testServiceProviderResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_SERVICE_PROVIDER, OslcConstants.PATH_SERVICE_PROVIDER);
	}

	public void testServiceProviderCatalogResourceShape()
		   throws URISyntaxException
	{
		testResourceShape(OslcConstants.TYPE_SERVICE_PROVIDER_CATALOG, OslcConstants.PATH_SERVICE_PROVIDER_CATALOG);
	}

	private void testResourceShape(final String type,
								   final String resourceShapePath)
			throws URISyntaxException
	{
		final String serviceProviderRegistryURI = ServiceProviderRegistryURIs.getServiceProviderRegistryURI();

		final int indexOf = serviceProviderRegistryURI.indexOf("/catalog");

		final String serviceProviderRegistryBaseURI = serviceProviderRegistryURI.substring(0,
																						   indexOf);

		final String uri = serviceProviderRegistryBaseURI +
						   "/" +
						   OslcConstants.PATH_RESOURCE_SHAPES +
						   "/" +
						   resourceShapePath;

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 uri);

		final ResourceShape resourceShape = oslcRestClient.getOslcResource(ResourceShape.class);

		verifyResourceShape(resourceShape,
							type,
							true);
	}

	protected void verifyResourceShape(final ResourceShape resourceShape,
									   final String		   domain,
									   final boolean	   recurse)
			  throws URISyntaxException
	{
		assertNotNull(resourceShape);

		final URI	about	  = resourceShape.getAbout();
		final URI[] describes = resourceShape.getDescribes();

		assertNotNull(about);
		assertNotNull(describes);
		assertTrue(describes.length > 0);

		if (domain != null)
		{
			assertTrue(Arrays.asList(describes).contains(new URI(domain)));
		}

		final Property[] properties = resourceShape.getProperties();

		assertNotNull(properties);
		assertTrue(properties.length > 0);

		for (final Property property : properties)
		{
			final String name				= property.getName();
			final URI	 propertyDefinition = property.getPropertyDefinition();

			assertNotNull(property.getDescription());
			assertNotNull(name);
			assertNotNull(property.getOccurs());
			assertNotNull(propertyDefinition);
			assertNotNull(property.isReadOnly());
			assertNotNull(property.getTitle());
			assertNotNull(property.getValueType());

			assertTrue("propertyDefinition [" + propertyDefinition.toString() + "], name [" + name + "]",
					   propertyDefinition.toString().endsWith(name));
		}

		if (recurse)
		{
			final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																	 about);

			final ResourceShape recursedResourceShape = oslcRestClient.getOslcResource(ResourceShape.class);

			verifyResourceShape(recursedResourceShape,
								domain,
								false);
		}
	}
}