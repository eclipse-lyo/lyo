/*******************************************************************************
 * Copyright (c) 2012, 2014 IBM Corporation.
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

import java.io.IOException;
import java.io.StringBufferInputStream;
import java.net.HttpURLConnection;
import java.net.URISyntaxException;

import javax.ws.rs.WebApplicationException;

import org.apache.wink.client.ClientResponse;
import org.eclipse.lyo.oslc4j.client.OslcRestClient;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreDuplicatePropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidOccursException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyTypeException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidRepresentationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidValueTypeException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingAnnotationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingSetMethodException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMisusedOccursException;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.OslcRdfXmlProvider;

import org.apache.jena.shared.JenaException;

@SuppressWarnings("deprecation")
public class TestInvalid
	   extends TestBase
{
	private static final String INVALID_RDF_XML = "<?xml version=\"1.0\"?>\n" +
												  "<rdf:RDF xmlns:rdf=\"" + OslcConstants.RDF_NAMESPACE + "\" " +
														   "xmlns:oslc=\"" + OslcConstants.OSLC_CORE_NAMESPACE + "\">\n" +
												  "	   <oslc:ServiceProvider>\n" +
												  "		   <oslc:service>\n" +
												  "			   <oslc:Service>\n" +
												  "				   <oslc:domain rdf:resource=\"http://open-services.net/ns/monitoring/1.0/\"/>\n" +
												  "			   </oslc:Service>\n" +
												  "			   <oslc:Service>\n" +
												  "				   <oslc:domain rdf:resource=\"http://open-services.net/ns/servicemanagement/1.0/sm#\"/>\n" +
												  "			   </oslc:Service>\n" +
												  "		   </oslc:service>\n" +
												  "	   </oslc:ServiceProvider>\n" +
												  "</rdf:RDF>";

	private static final String INVALID_RDF_XML_EXACTLY_ONE = "<?xml version=\"1.0\"?>\n" +
															  "<rdf:RDF xmlns:rdf=\"" + OslcConstants.RDF_NAMESPACE + "\" " +
																	   "xmlns:oslc=\"" + OslcConstants.OSLC_CORE_NAMESPACE + "\" " +
																	   "xmlns:dcterms=\"" + OslcConstants.DCTERMS_NAMESPACE + "\">\n" +
															  "<oslc:Compact>\n" +
															  "	   <dcterms:title>Title1</dcterms:title>\n" +
															  "	   <dcterms:title>Title2</dcterms:title>\n" +
															  "</oslc:Compact>\n" +
															  "</rdf:RDF>";

	private static final String INVALID_RDF_XML_ZERO_OR_ONE = "<?xml version=\"1.0\"?>\n" +
															  "<rdf:RDF xmlns:rdf=\"" + OslcConstants.RDF_NAMESPACE + "\" " +
																	   "xmlns:oslc=\"" + OslcConstants.OSLC_CORE_NAMESPACE + "\" " +
																	   "xmlns:dcterms=\"" + OslcConstants.DCTERMS_NAMESPACE + "\">\n" +
															  "<oslc:Compact>\n" +
															  "	   <dcterms:title>Title1</dcterms:title>\n" +
															  "	   <oslc:shortTitle>ShortTitle1</oslc:shortTitle>\n" +
															  "	   <oslc:shortTitle>ShortTitle2</oslc:shortTitle>\n" +
															  "</oslc:Compact>\n" +
															  "</rdf:RDF>";

	private static final String INVALID_RDF_XML_SERVICE_PROVIDER_ZERO_OR_ONE = "<?xml version=\"1.0\"?>\n" +
																			   "<rdf:RDF xmlns:rdf=\"" + OslcConstants.RDF_NAMESPACE + "\" " +
																			   "xmlns:oslc=\"" + OslcConstants.OSLC_CORE_NAMESPACE + "\" " +
																			   "xmlns:dcterms=\"" + OslcConstants.DCTERMS_NAMESPACE + "\">\n" +
																			   "<oslc:ServiceProvider>\n" +
																			   "	<dcterms:title>Title1</dcterms:title>\n" +
																			   "	<dcterms:title>Title2</dcterms:title>\n" +
																			   "	<oslc:service>\n" +
																			   "		<oslc:Service>\n" +
																			   "			<oslc:domain rdf:resource=\"http://ibm.com/ns/test#\"/>\n" +
																			   "		</oslc:Service>\n" +
																			   "	</oslc:service>\n" +
																			   "</oslc:ServiceProvider>\n" +
																			   "</rdf:RDF>";

	@SuppressWarnings({
		"unchecked",
		"rawtypes"
	})
	public void testInvalidRdfXml()
		   throws IOException
	{
		final StringBufferInputStream stringBufferInputStream = new StringBufferInputStream(INVALID_RDF_XML);

		final OslcRdfXmlProvider oslcRdfXmlProvider = new OslcRdfXmlProvider();

		try
		{
			oslcRdfXmlProvider.readFrom((Class) ServiceProvider.class,
										null,
										null,
										OslcMediaType.APPLICATION_RDF_XML_TYPE,
										null,
										stringBufferInputStream);

			fail();
		}
		catch (final WebApplicationException exception)
		{
			verifyWebApplicationException(exception,
										  JenaException.class);
		}
	}

	@SuppressWarnings({
		"unchecked",
		"rawtypes"
	})
	public void testInvalidRdfXmlExactlyOne()
		   throws IOException
	{
		final StringBufferInputStream stringBufferInputStream = new StringBufferInputStream(INVALID_RDF_XML_EXACTLY_ONE);

		final OslcRdfXmlProvider oslcRdfXmlProvider = new OslcRdfXmlProvider();

		try
		{
			oslcRdfXmlProvider.readFrom((Class) Compact.class,
										null,
										null,
										OslcMediaType.APPLICATION_RDF_XML_TYPE,
										null,
										stringBufferInputStream);

			fail();
		}
		catch (final WebApplicationException exception)
		{
			verifyWebApplicationException(exception,
										  OslcCoreMisusedOccursException.class);
		}
	}

	@SuppressWarnings({
		"unchecked",
		"rawtypes"
	})
	public void testInvalidRdfXmlZeroOrOne()
		   throws IOException
	{
		final StringBufferInputStream stringBufferInputStream = new StringBufferInputStream(INVALID_RDF_XML_ZERO_OR_ONE);

		final OslcRdfXmlProvider oslcRdfXmlProvider = new OslcRdfXmlProvider();

		try
		{
			oslcRdfXmlProvider.readFrom((Class) Compact.class,
										null,
										null,
										OslcMediaType.APPLICATION_RDF_XML_TYPE,
										null,
										stringBufferInputStream);

			fail();
		}
		catch (final WebApplicationException exception)
		{
			verifyWebApplicationException(exception,
										  OslcCoreMisusedOccursException.class);
		}
	}

	public void testInvalidServiceProviderServerMessageBodyReaderZeroOrOne()
	{
		final String creation = getCreation(OslcMediaType.APPLICATION_RDF_XML,
											OslcConstants.OSLC_CORE_DOMAIN,
											OslcConstants.TYPE_SERVICE_PROVIDER);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 creation,
																 OslcMediaType.APPLICATION_RDF_XML);

		final ClientResponse clientResponse = oslcRestClient.addOslcResourceReturnClientResponse(INVALID_RDF_XML_SERVICE_PROVIDER_ZERO_OR_ONE);

		final Error error = clientResponse.getEntity(Error.class);

		assertNotNull(error);

		assertEquals(String.valueOf(HttpURLConnection.HTTP_BAD_REQUEST), error.getStatusCode());
		assertNotNull(error.getMessage());
	}

	public void testInvalidResourceShapeArrayPropertyType()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidArrayPropertyType.class);

			fail();
		}
		catch (final OslcCoreInvalidPropertyTypeException exception)
		{
		}
	}

	public void testInvalidResourceShapeCollectonPropertyType()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidCollectionPropertyType.class);

			fail();
		}
		catch (final OslcCoreInvalidPropertyTypeException exception)
		{
		}
	}

	public void testInvalidResourceShapeDuplicatePropertyDefinition()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidDuplicatePropertyDefinition.class);

			fail();
		}
		catch (final OslcCoreDuplicatePropertyDefinitionException exception)
		{
		}
	}

	public void testInvalidResourceShapeMissingSetMethod()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidMissingSetMethod.class);

			fail();
		}
		catch (final OslcCoreMissingSetMethodException exception)
		{
		}
	}

	public void testInvalidResourceShapeOccurs1()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidOccurs1.class);

			fail();
		}
		catch (final OslcCoreInvalidOccursException exception)
		{
		}
	}

	public void testInvalidResourceShapeOccurs2()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidOccurs2.class);

			fail();
		}
		catch (final OslcCoreInvalidOccursException exception)
		{
		}
	}

	public void testInvalidResourceShapeOccurs3()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidOccurs3.class);

			fail();
		}
		catch (final OslcCoreInvalidOccursException exception)
		{
		}
	}

	public void testInvalidResourceShapeOccurs4()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidOccurs4.class);

			fail();
		}
		catch (final OslcCoreInvalidOccursException exception)
		{
		}
	}

	public void testInvalidResourceShapeOccurs5()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidOccurs5.class);

			fail();
		}
		catch (final OslcCoreInvalidOccursException exception)
		{
		}
	}

	public void testInvalidResourceShapeOccurs6()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidOccurs6.class);

			fail();
		}
		catch (final OslcCoreInvalidOccursException exception)
		{
		}
	}

	public void testInvalidResourceShapePropertyDefinition()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidPropertyDefinition.class);

			fail();
		}
		catch (final OslcCoreInvalidPropertyDefinitionException exception)
		{
		}
	}

	public void testInvalidResourceShapePropertyType()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidPropertyType.class);

			fail();
		}
		catch (final OslcCoreInvalidPropertyTypeException exception)
		{
		}
	}

	public void testInvalidResourceShapePropertyTypeNested()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidPropertyTypeNested.class);

			fail();
		}
		catch (final OslcCoreMissingAnnotationException exception)
		{
		}
	}

	public void testInvalidResourceShapeRepresentation1()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidRepresentation1.class);

			fail();
		}
		catch (final OslcCoreInvalidRepresentationException exception)
		{
		}
	}

	public void testInvalidResourceShapeRepresentation2()
			throws OslcCoreApplicationException,
				   URISyntaxException
	 {
		 try
		 {
			 ResourceShapeFactory.createResourceShape("http://bogus",
													  OslcConstants.PATH_RESOURCE_SHAPES,
													  "bogus",
													  InvalidRepresentation2.class);

			 fail();
		 }
		 catch (final OslcCoreInvalidRepresentationException exception)
		 {
		 }
	 }

	 public void testInvalidResourceShapeValueType1()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidValueType1.class);

			fail();
		}
		catch (final OslcCoreInvalidValueTypeException exception)
		{
		}
	}

	public void testInvalidResourceShapeValueType2()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidValueType2.class);

			fail();
		}
		catch (final OslcCoreInvalidValueTypeException exception)
		{
		}
	}

	public void testInvalidResourceShapeValueType3()
		   throws OslcCoreApplicationException,
				  URISyntaxException
	{
		try
		{
			ResourceShapeFactory.createResourceShape("http://bogus",
													 OslcConstants.PATH_RESOURCE_SHAPES,
													 "bogus",
													 InvalidValueType3.class);

			fail();
		}
		catch (final OslcCoreInvalidValueTypeException exception)
		{
		}
	}
}
