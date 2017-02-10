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

import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.ws.rs.core.Response;
import javax.xml.namespace.QName;

import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.ClientWebException;
import org.apache.wink.client.EntityType;
import org.eclipse.lyo.oslc4j.client.OslcRestClient;
import org.eclipse.lyo.oslc4j.core.model.AnyResource;
import org.eclipse.lyo.oslc4j.core.model.Compact;
import org.eclipse.lyo.oslc4j.core.model.Error;
import org.eclipse.lyo.oslc4j.core.model.ExtendedError;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.Preview;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;
import org.eclipse.lyo.oslc4j.test.Constants;
import org.eclipse.lyo.oslc4j.test.Nested;
import org.eclipse.lyo.oslc4j.test.Test;

public abstract class TestTypesBase
	   extends TestBase
{
	private static final BigInteger				VALUE_BIG_INTEGER				= new BigInteger("1");
	private static final BigInteger[]			VALUE_BIG_INTEGERS				= new BigInteger[] { VALUE_BIG_INTEGER };
	private static final Boolean				VALUE_BOOLEAN					= Boolean.TRUE;
	private static final Boolean[]				VALUE_BOOLEANS					= new Boolean[] { VALUE_BOOLEAN };
	private static final Byte					VALUE_BYTE						= Byte.valueOf("2");
	private static final Byte[]					VALUE_BYTES						= new Byte[] { VALUE_BYTE };
	static private final Date					VALUE_DATE						= new Date();
	static private final Date[]					VALUE_DATES						= new Date[] { VALUE_DATE };
	private static final Double					VALUE_DOUBLE					= Double.valueOf(3);
	private static final Double[]				VALUE_DOUBLES					= new Double[] { VALUE_DOUBLE };
	private static final Float					VALUE_FLOAT						= Float.valueOf(4);
	private static final Float[]				VALUE_FLOATS					= new Float[] { VALUE_FLOAT };
	private static final Integer				VALUE_INTEGER					= Integer.valueOf(5);
	private static final Integer[]				VALUE_INTEGERS					= new Integer[] { VALUE_INTEGER };
	private static final Long					VALUE_LONG						= Long.valueOf(6);
	private static final Long[]					VALUE_LONGS						= new Long[] { VALUE_LONG };
	private static final Nested					VALUE_NESTED					= new Nested();
	private static final Nested[]				VALUE_NESTEDS					= new Nested[] { VALUE_NESTED };
	private static final boolean				VALUE_PRIMITIVE_BOOLEAN			= true;
	private static final boolean[]				VALUE_PRIMITIVE_BOOLEANS		= new boolean[] { VALUE_PRIMITIVE_BOOLEAN };
	private static final byte					VALUE_PRIMITIVE_BYTE			= (byte) 7;
	private static final byte[]					VALUE_PRIMITIVE_BYTES			= new byte[] { VALUE_PRIMITIVE_BYTE };
	private static final double					VALUE_PRIMITIVE_DOUBLE			= 8.0D;
	private static final double[]				VALUE_PRIMITIVE_DOUBLES			= new double[] { VALUE_PRIMITIVE_DOUBLE };
	private static final float					VALUE_PRIMITIVE_FLOAT			= 9.0F;
	private static final float[]				VALUE_PRIMITIVE_FLOATS			= new float[] { VALUE_PRIMITIVE_FLOAT };
	private static final int					VALUE_PRIMITIVE_INTEGER			= 10;
	private static final int[]					VALUE_PRIMITIVE_INTEGERS		= new int[] { VALUE_PRIMITIVE_INTEGER };
	private static final long					VALUE_PRIMITIVE_LONG			= 11;
	private static final long[]					VALUE_PRIMITIVE_LONGS			= new long[] { VALUE_PRIMITIVE_LONG };
	private static final short					VALUE_PRIMITIVE_SHORT			= (short) 12;
	private static final short[]				VALUE_PRIMITIVE_SHORTS			= new short[] { VALUE_PRIMITIVE_SHORT };
	private static final Short					VALUE_SHORT						= Short.valueOf((short) 13);
	private static final Short[]				VALUE_SHORTS					= new Short[] { VALUE_SHORT };
	private static final String					VALUE_STRING					= "Hello World!";
	private static final String[]				VALUE_STRINGS					= new String[] { VALUE_STRING };
	private static final URI					VALUE_URI;
	private static final URI[]					VALUE_URIS;
	private static final String					VALUE_XML_LITERAL				= "Hello <b>World</b>!";
	private static final String					CUSTOM_NAMESPACE				= "http://example.com/ns#";
	private static final String					CUSTOM_PREFIX					= "ex"; 
	private static final Map<QName, Object>		CUSTOM_FIELDS					= new HashMap<QName, Object>();
	private static final URI					CUSTOM_TYPE;
	private static final AnyResource			ANY_RESOURCE					= new AnyResource();
	
	private static URI CREATED_TEST_URI;

	static
	{
		VALUE_NESTED.setStringProperty("Goodbye");

		try
		{
			VALUE_URI  = new URI("http://www.ibm.com");
			VALUE_URIS = new URI[] {VALUE_URI};
			
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customString", CUSTOM_PREFIX), VALUE_STRING);
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customBool", CUSTOM_PREFIX), Boolean.TRUE);
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customDate", CUSTOM_PREFIX), new Date());
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customURIRef", CUSTOM_PREFIX), new URI("http://example.com/resources/3"));
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customInt", CUSTOM_PREFIX), VALUE_INTEGER);
			
			final List<String> stringCollection = Arrays.asList("one",
																"two",
																"three");
			
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customStringCollection", CUSTOM_PREFIX), stringCollection);
			
			final List<URI> uriCollection = Arrays.asList(new URI("http://example.com/resources/27"),
														  new URI("http://example.com/resources/13"));
			
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customURICollection", CUSTOM_PREFIX), uriCollection);

			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customBigInt", CUSTOM_PREFIX), VALUE_BIG_INTEGER);
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customFloat", CUSTOM_PREFIX), VALUE_FLOAT);
			
			// Inline resource with a URI
			final Nested nested1 = new Nested();
			nested1.setAbout(new URI("http://example.com/resources/42"));
			nested1.setStringProperty("foo");
			
			final Nested nested2 = new Nested();
			nested2.setAbout(new URI("http://example.com/resources/109"));
			nested2.setStringProperty("bar");
			
			// Individual resource
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customInlineResource", CUSTOM_PREFIX), nested1);
			
			// Collection
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customInlineResourceCollection", CUSTOM_PREFIX), Arrays.asList(nested1, nested2));
			
			// Blank node
			final Nested nested3 = new Nested();
			nested3.setStringProperty("baz");
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customBlankNode", CUSTOM_PREFIX), nested3);
			
			// XMLLiteral
			final XMLLiteral customXMLLiteral = new XMLLiteral("Hello <b>World!</b>");
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "customXMLLiteral", CUSTOM_PREFIX), customXMLLiteral);
			
			// Custom RDF types
			CUSTOM_TYPE = new URI(CUSTOM_NAMESPACE + "CustomType");
			
			// "Any" resource
			ANY_RESOURCE.addType(CUSTOM_TYPE);
			ANY_RESOURCE.getExtendedProperties().put(new QName(CUSTOM_NAMESPACE, "anyResourceProperty", CUSTOM_PREFIX), "Nested property of an 'any' resource.");
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "anyResource", CUSTOM_PREFIX), ANY_RESOURCE);
			
			// Multiple types
			final Nested nested4 = new Nested();
			nested4.addType(CUSTOM_TYPE);
			nested4.setStringProperty("This resource has more than one rdf:type.");
			CUSTOM_FIELDS.put(new QName(CUSTOM_NAMESPACE, "multipleTypes", CUSTOM_PREFIX), nested4);
		}
		catch (final URISyntaxException exception)
		{
			throw new ExceptionInInitializerError(exception);
		}
	}

	private static final Collection<BigInteger> VALUE_BIG_INTEGER_COLLECTION = new ArrayList<BigInteger>(Arrays.asList(VALUE_BIG_INTEGERS));
	private static final List<Boolean>			VALUE_BOOLEAN_COLLECTION	 = new ArrayList<Boolean>(Arrays.asList(VALUE_BOOLEANS));
	private static final Set<Byte>				VALUE_BYTE_COLLECTION		 = new TreeSet<Byte>(Arrays.asList(VALUE_BYTES));
	private static final SortedSet<Date>		VALUE_DATE_COLLECTION		 = new TreeSet<Date>(Arrays.asList(VALUE_DATES));
	private static final ArrayList<Double>		VALUE_DOUBLE_COLLECTION		 = new ArrayList<Double>(Arrays.asList(VALUE_DOUBLES));
	private static final HashSet<Float>			VALUE_FLOAT_COLLECTION		 = new HashSet<Float>(Arrays.asList(VALUE_FLOATS));
	private static final TreeSet<Integer>		VALUE_INTEGER_COLLECTION	 = new TreeSet<Integer>(Arrays.asList(VALUE_INTEGERS));
	private static final Collection<Long>		VALUE_LONG_COLLECTION		 = new ArrayList<Long>(Arrays.asList(VALUE_LONGS));
	private static final Collection<Nested>		VALUE_NESTED_COLLECTION		 = new ArrayList<Nested>(Arrays.asList(VALUE_NESTEDS));
	private static final Collection<Short>		VALUE_SHORT_COLLECTION		 = new ArrayList<Short>(Arrays.asList(VALUE_SHORTS));
	private static final Collection<String>		VALUE_STRING_COLLECTION		 = new ArrayList<String>(Arrays.asList(VALUE_STRINGS));
	private static final Collection<URI>		VALUE_URI_COLLECTION		 = new ArrayList<URI>(Arrays.asList(VALUE_URIS));

	protected TestTypesBase()
	{
		super();
	}

	protected static final void testCreate(final String mediaType)
	{
		CREATED_TEST_URI = null;

		final Test test = new Test();

		test.setBigIntegerCollection(VALUE_BIG_INTEGER_COLLECTION);
		test.setBigIntegerProperty(VALUE_BIG_INTEGER);
		test.setBigIntegerProperties(VALUE_BIG_INTEGERS);
		test.setBooleanCollection(VALUE_BOOLEAN_COLLECTION);
		test.setBooleanProperty(VALUE_BOOLEAN);
		test.setBooleanProperties(VALUE_BOOLEANS);
		test.setByteCollection(VALUE_BYTE_COLLECTION);
		test.setByteProperty(VALUE_BYTE);
		test.setByteProperties(VALUE_BYTES);
		test.setDateCollection(VALUE_DATE_COLLECTION);
		test.setDateProperty(VALUE_DATE);
		test.setDateProperties(VALUE_DATES);
		test.setDoubleCollection(VALUE_DOUBLE_COLLECTION);
		test.setDoubleProperty(VALUE_DOUBLE);
		test.setDoubleProperties(VALUE_DOUBLES);
		test.setFloatCollection(VALUE_FLOAT_COLLECTION);
		test.setFloatProperty(VALUE_FLOAT);
		test.setFloatProperties(VALUE_FLOATS);
		test.setIntegerCollection(VALUE_INTEGER_COLLECTION);
		test.setIntegerProperty(VALUE_INTEGER);
		test.setIntegerProperties(VALUE_INTEGERS);
		test.setLongCollection(VALUE_LONG_COLLECTION);
		test.setLongProperty(VALUE_LONG);
		test.setLongProperties(VALUE_LONGS);
		test.setNestedCollection(VALUE_NESTED_COLLECTION);
		test.setNestedProperty(VALUE_NESTED);
		test.setNestedProperties(VALUE_NESTEDS);
		test.setPrimitiveBooleanProperty(VALUE_PRIMITIVE_BOOLEAN);
		test.setPrimitiveBooleanProperties(VALUE_PRIMITIVE_BOOLEANS);
		test.setPrimitiveByteProperty(VALUE_PRIMITIVE_BYTE);
		test.setPrimitiveByteProperties(VALUE_PRIMITIVE_BYTES);
		test.setPrimitiveDoubleProperty(VALUE_PRIMITIVE_DOUBLE);
		test.setPrimitiveDoubleProperties(VALUE_PRIMITIVE_DOUBLES);
		test.setPrimitiveFloatProperty(VALUE_PRIMITIVE_FLOAT);
		test.setPrimitiveFloatProperties(VALUE_PRIMITIVE_FLOATS);
		test.setPrimitiveIntegerProperty(VALUE_PRIMITIVE_INTEGER);
		test.setPrimitiveIntegerProperties(VALUE_PRIMITIVE_INTEGERS);
		test.setPrimitiveLongProperty(VALUE_PRIMITIVE_LONG);
		test.setPrimitiveLongProperties(VALUE_PRIMITIVE_LONGS);
		test.setPrimitiveShortProperty(VALUE_PRIMITIVE_SHORT);
		test.setPrimitiveShortProperties(VALUE_PRIMITIVE_SHORTS);
		test.setShortCollection(VALUE_SHORT_COLLECTION);
		test.setShortProperty(VALUE_SHORT);
		test.setShortProperties(VALUE_SHORTS);
		test.setStringCollection(VALUE_STRING_COLLECTION);
		test.setStringProperty(VALUE_STRING);
		test.setStringProperties(VALUE_STRINGS);
		test.setUriCollection(VALUE_URI_COLLECTION);
		test.setUriProperty(VALUE_URI);
		test.setUriProperties(VALUE_URIS);
		test.setXmlLiteralProperty(VALUE_XML_LITERAL);
 
		for (Entry<QName, Object> customField : CUSTOM_FIELDS.entrySet())
		{
			test.getExtendedProperties().put(customField.getKey(), customField.getValue());
		}
		
		// Special numbers
		test.setDoubleNaNProperty(Double.NaN);
		test.setDoubleNegativeInfinityProperty(Double.NEGATIVE_INFINITY);
		test.setDoublePositiveInfinityProperty(Double.POSITIVE_INFINITY);
		test.setFloatNaNProperty(Float.NaN);
		test.setFloatNegativeInfinityProperty(Float.NEGATIVE_INFINITY);
		test.setFloatPositiveInfinityProperty(Float.POSITIVE_INFINITY);
		test.setPrimitiveDoubleNaNProperty(Double.NaN);
		test.setPrimitiveDoubleNegativeInfinityProperty(Double.NEGATIVE_INFINITY);
		test.setPrimitiveDoublePositiveInfinityProperty(Double.POSITIVE_INFINITY);
		test.setPrimitiveFloatNaNProperty(Float.NaN);
		test.setPrimitiveFloatNegativeInfinityProperty(Float.NEGATIVE_INFINITY);
		test.setPrimitiveFloatPositiveInfinityProperty(Float.NEGATIVE_INFINITY);

		final String creation = getCreation(mediaType,
											Constants.TEST_DOMAIN,
											Constants.TYPE_TEST);

		assertNotNull(creation);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 creation,
																 mediaType);

		final Test addedTest = oslcRestClient.addOslcResource(test);

		verifyTest(mediaType,
				   addedTest,
				   true);

		CREATED_TEST_URI = addedTest.getAbout();
	}

	protected static final void testDelete(final String mediaType)
	{
		assertNotNull(CREATED_TEST_URI);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 CREATED_TEST_URI,
																 mediaType);

		final ClientResponse clientResponse = oslcRestClient.removeOslcResourceReturnClientResponse();

		assertNotNull(clientResponse);
		assertEquals(HttpURLConnection.HTTP_NO_CONTENT, clientResponse.getStatusCode());

		assertNull(oslcRestClient.getOslcResource(Test.class));

		CREATED_TEST_URI = null;
	}

	protected static final void testResourceShape(final String mediaType)
			  throws URISyntaxException
	{
		final ResourceShape resourceShape = getResourceShape(mediaType,
															 Constants.TYPE_TEST);

		verifyResourceShape(resourceShape,
							Constants.TYPE_TEST);
	}

	protected static final void testRetrieve(final String mediaType)
	{
		assertNotNull(CREATED_TEST_URI);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 CREATED_TEST_URI,
																 mediaType);

		final Test test = oslcRestClient.getOslcResource(Test.class);

		verifyTest(mediaType,
				   test,
				   true);
	}

	protected static final void testRetrieveCompact(final String mediaType)
	{
		assertNotNull(CREATED_TEST_URI);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 CREATED_TEST_URI,
																 mediaType);

		final Compact compact = oslcRestClient.getOslcResource(Compact.class);

		verifyCompact(mediaType,
					  compact,
					  true);
	}

	protected static final void testRetrieveCollection(final String mediaType)
	{
		assertNotNull(CREATED_TEST_URI);

		final String queryBase = getQueryBase(mediaType,
											  Constants.USAGE_COLLECTION,
											  Constants.TYPE_TEST);

		assertNotNull(queryBase);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 queryBase,
																 mediaType);

		final Collection<Test> testCollection = oslcRestClient.getOslcResources(new EntityType<Collection<Test>>(){});

		assertNotNull(testCollection);
		assertTrue(testCollection.size() > 0);

		boolean found = false;

		for (final Test test : testCollection)
		{
			if (CREATED_TEST_URI.equals(test.getAbout()))
			{
				found = true;

				verifyTest(mediaType,
						   test,
						   true);
			}
		}

		assertTrue(found);
	}

	protected static final void testRetrieveError(final String mediaType)
	{
		final String queryBase = getQueryBase(mediaType,
											  Constants.USAGE_ERROR,
											  Constants.TYPE_TEST);

		assertNotNull(queryBase);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 queryBase,
																 mediaType);

		final Error testError = oslcRestClient.getOslcResource(Error.class);

		assertNotNull(testError);

		final ExtendedError extendedError = testError.getExtendedError();
		final String		message		  = testError.getMessage();
		final String		statusCode	  = testError.getStatusCode();

		assertNotNull(extendedError);
		assertNotNull(message);
		assertNotNull(statusCode);

		assertEquals(String.valueOf(Response.Status.BAD_REQUEST.getStatusCode()), statusCode);

		final String hintHeight = extendedError.getHintHeight();
		final String hintWidth	= extendedError.getHintWidth();
		final URI	 moreInfo	= extendedError.getMoreInfo();
		final String rel		= extendedError.getRel();

		assertNotNull(hintHeight);
		assertNotNull(hintWidth);
		assertNotNull(moreInfo);
		assertNotNull(rel);
	}

	protected static final void testRetrieveMessageBodyWriterError(final String mediaType)
	{
		final String queryBase = getQueryBase(mediaType,
											  Constants.USAGE_MESSAGE_BODY_WRITER_ERROR,
											  Constants.TYPE_TEST);

		assertNotNull(queryBase);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 queryBase,
																 mediaType);

		try
		{
			@SuppressWarnings("unused")
			final Test[] tests = oslcRestClient.getOslcResources(Test[].class);

			fail();
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			final int statusCode = response.getStatusCode();
			assertEquals(HttpURLConnection.HTTP_INTERNAL_ERROR, statusCode);
		}
	}

	protected static final void testRetrieveCompactMessageBodyWriterError(final String compactMediaType,
																		  final String resourceMediaType)
	{
		final String queryBase = getQueryBase(resourceMediaType,
											  Constants.USAGE_MESSAGE_BODY_WRITER_ERROR,
											  Constants.TYPE_TEST);

		assertNotNull(queryBase);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 queryBase + "/4",
																 compactMediaType);

		try
		{
			@SuppressWarnings("unused")
			final Compact compact = oslcRestClient.getOslcResource(Compact.class);

			fail();
		}
		catch (final ClientWebException exception)
		{
			final ClientResponse response = exception.getResponse();

			final int statusCode = response.getStatusCode();
			assertEquals(HttpURLConnection.HTTP_INTERNAL_ERROR, statusCode);
		}
	}

	protected static final void testRetrieves(final String mediaType)
	{
		assertNotNull(CREATED_TEST_URI);

		final String queryBase = getQueryBase(mediaType,
											  OslcConstants.OSLC_USAGE_DEFAULT,
											  Constants.TYPE_TEST);

		assertNotNull(queryBase);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 queryBase,
																 mediaType);

		final Test[] tests = oslcRestClient.getOslcResources(Test[].class);

		assertNotNull(tests);
		assertTrue(tests.length > 0);

		boolean found = false;

		for (final Test test : tests)
		{
			if (CREATED_TEST_URI.equals(test.getAbout()))
			{
				found = true;

				verifyTest(mediaType,
						   test,
						   true);
			}
		}

		assertTrue(found);
	}

	protected static final void testUpdate(final String mediaType)
	{
		assertNotNull(CREATED_TEST_URI);

		final OslcRestClient oslcRestClient = new OslcRestClient(PROVIDERS,
																 CREATED_TEST_URI,
																 mediaType);

		final Test test = oslcRestClient.getOslcResource(Test.class);

		verifyTest(mediaType,
				   test,
				   true);

		final ClientResponse clientResponse = oslcRestClient.updateOslcResourceReturnClientResponse(test);

		assertNotNull(clientResponse);
		assertEquals(HttpURLConnection.HTTP_OK, clientResponse.getStatusCode());

		final Test updatedTest = oslcRestClient.getOslcResource(Test.class);

		verifyTest(mediaType,
				   test,
				   true);

		assertEquals(test.getAbout(), updatedTest.getAbout());
	}
   
	protected static void verifyTest(final String  mediaType,
									 final Test	   test,
									 final boolean recurse)
	{
		assertNotNull(test);

		final URI					 aboutURI					= test.getAbout();
		final Collection<BigInteger> bigIntegerCollection		= test.getBigIntegerCollection();
		final BigInteger			 bigIntegerProperty			= test.getBigIntegerProperty();
		final BigInteger[]			 bigIntegerProperties		= test.getBigIntegerProperties();
		final List<Boolean>			 booleanCollection			= test.getBooleanCollection();
		final Boolean				 booleanProperty			= test.isBooleanProperty();
		final Boolean[]				 booleanProperties			= test.getBooleanProperties();
		final Set<Byte>				 byteCollection				= test.getByteCollection();
		final Byte					 byteProperty				= test.getByteProperty();
		final Byte[]				 byteProperties				= test.getByteProperties();
		final SortedSet<Date>		 dateCollection				= test.getDateCollection();
		final Date					 dateProperty				= test.getDateProperty();
		final Date[]				 dateProperties				= test.getDateProperties();
		final ArrayList<Double>		 doubleCollection			= test.getDoubleCollection();
		final Double				 doubleProperty				= test.getDoubleProperty();
		final Double[]				 doubleProperties			= test.getDoubleProperties();
		final HashSet<Float>		 floatCollection			= test.getFloatCollection();
		final Float					 floatProperty				= test.getFloatProperty();
		final Float[]				 floatProperties			= test.getFloatProperties();
		final String				 identifierString			= test.getIdentifier();
		final TreeSet<Integer>		 integerCollection			= test.getIntegerCollection();
		final Integer				 integerProperty			= test.getIntegerProperty();
		final Integer[]				 integerProperties			= test.getIntegerProperties();
		final Collection<Long>		 longCollection				= test.getLongCollection();
		final Long					 longProperty				= test.getLongProperty();
		final Long[]				 longProperties				= test.getLongProperties();
		final Collection<Nested>	 nestedCollection			= test.getNestedCollection();
		final Nested				 nestedProperty				= test.getNestedProperty();
		final Nested[]				 nestedProperties			= test.getNestedProperties();
		final boolean				 primitiveBooleanProperty	= test.isPrimitiveBooleanProperty();
		final boolean[]				 primitiveBooleanProperties = test.getPrimitiveBooleanProperties();
		final byte					 primitiveByteProperty		= test.getPrimitiveByteProperty();
		final byte[]				 primitiveByteProperties	= test.getPrimitiveByteProperties();
		final double				 primitiveDoubleProperty	= test.getPrimitiveDoubleProperty();
		final double[]				 primitiveDoubleProperties	= test.getPrimitiveDoubleProperties();
		final float					 primitiveFloatProperty		= test.getPrimitiveFloatProperty();
		final float[]				 primitiveFloatProperties	= test.getPrimitiveFloatProperties();
		final int					 primitiveIntegerProperty	= test.getPrimitiveIntegerProperty();
		final int[]					 primitiveIntegerProperties = test.getPrimitiveIntegerProperties();
		final long					 primitiveLongProperty		= test.getPrimitiveLongProperty();
		final long[]				 primitiveLongProperties	= test.getPrimitiveLongProperties();
		final short					 primitiveShortProperty		= test.getPrimitiveShortProperty();
		final short[]				 primitiveShortProperties	= test.getPrimitiveShortProperties();
		final Collection<Short>		 shortCollection			= test.getShortCollection();
		final Short					 shortProperty				= test.getShortProperty();
		final Short[]				 shortProperties			= test.getShortProperties();
		final Collection<String>	 stringCollection			= test.getStringCollection();
		final String				 stringProperty				= test.getStringProperty();
		final String[]				 stringProperties			= test.getStringProperties();
		final Collection<URI>		 uriCollection				= test.getUriCollection();
		final URI					 uriProperty				= test.getUriProperty();
		final URI[]					 uriProperties				= test.getUriProperties();

		assertNotNull(aboutURI);
		assertNotNull(identifierString);
		assertNotNull(nestedProperty);

		assertTrue(aboutURI.toString().endsWith(identifierString));

		assertEquals(VALUE_BIG_INTEGER, bigIntegerProperty);
		assertEquals(VALUE_BIG_INTEGER_COLLECTION, bigIntegerCollection);
		assertTrue(Arrays.equals(VALUE_BIG_INTEGERS, bigIntegerProperties));
		assertEquals(VALUE_BOOLEAN, booleanProperty);
		assertEquals(VALUE_BOOLEAN_COLLECTION, booleanCollection);
		assertTrue(Arrays.equals(VALUE_BOOLEANS, booleanProperties));
		assertEquals(VALUE_BYTE, byteProperty);
		assertEquals(VALUE_BYTE_COLLECTION, byteCollection);
		assertTrue(Arrays.equals(VALUE_BYTES, byteProperties));
		assertEquals(VALUE_DATE, dateProperty);
		assertEquals(VALUE_DATE_COLLECTION, dateCollection);
		assertTrue(Arrays.equals(VALUE_DATES, dateProperties));
		assertEquals(VALUE_DOUBLE, doubleProperty);
		assertEquals(VALUE_DOUBLE_COLLECTION, doubleCollection);
		assertTrue(Arrays.equals(VALUE_DOUBLES, doubleProperties));
		assertEquals(VALUE_FLOAT, floatProperty);
		assertEquals(VALUE_FLOAT_COLLECTION, floatCollection);
		assertTrue(Arrays.equals(VALUE_FLOATS, floatProperties));
		assertEquals(VALUE_INTEGER, integerProperty);
		assertEquals(VALUE_INTEGER_COLLECTION, integerCollection);
		assertTrue(Arrays.equals(VALUE_INTEGERS, integerProperties));
		assertEquals(VALUE_LONG, longProperty);
		assertEquals(VALUE_LONG_COLLECTION, longCollection);
		assertTrue(Arrays.equals(VALUE_LONGS, longProperties));
		assertEquals(VALUE_NESTED.getStringProperty(), nestedProperty.getStringProperty());
		assertNotNull(nestedProperties);
		assertEquals(VALUE_NESTED_COLLECTION.size(), nestedCollection.size());
		assertEquals(VALUE_NESTED_COLLECTION.iterator().next().getStringProperty(), nestedCollection.iterator().next().getStringProperty());
		assertEquals(VALUE_NESTEDS.length, nestedProperties.length);
		assertEquals(VALUE_NESTEDS[0].getStringProperty(), nestedProperties[0].getStringProperty());
		assertEquals(VALUE_PRIMITIVE_BOOLEAN, primitiveBooleanProperty);
		assertTrue(Arrays.equals(VALUE_PRIMITIVE_BOOLEANS, primitiveBooleanProperties));
		assertEquals(VALUE_PRIMITIVE_BYTE, primitiveByteProperty);
		assertTrue(Arrays.equals(VALUE_PRIMITIVE_BYTES, primitiveByteProperties));
		assertEquals(VALUE_PRIMITIVE_DOUBLE, primitiveDoubleProperty, 0.0D);
		assertTrue(Arrays.equals(VALUE_PRIMITIVE_DOUBLES, primitiveDoubleProperties));
		assertEquals(VALUE_PRIMITIVE_FLOAT, primitiveFloatProperty, 0.0F);
		assertTrue(Arrays.equals(VALUE_PRIMITIVE_FLOATS, primitiveFloatProperties));
		assertEquals(VALUE_PRIMITIVE_INTEGER, primitiveIntegerProperty);
		assertTrue(Arrays.equals(VALUE_PRIMITIVE_INTEGERS, primitiveIntegerProperties));
		assertEquals(VALUE_PRIMITIVE_LONG, primitiveLongProperty);
		assertTrue(Arrays.equals(VALUE_PRIMITIVE_LONGS, primitiveLongProperties));
		assertEquals(VALUE_PRIMITIVE_SHORT, primitiveShortProperty);
		assertTrue(Arrays.equals(VALUE_PRIMITIVE_SHORTS, primitiveShortProperties));
		assertEquals(VALUE_SHORT, shortProperty);
		assertEquals(VALUE_SHORT_COLLECTION, shortCollection);
		assertTrue(Arrays.equals(VALUE_SHORTS, shortProperties));
		assertEquals(VALUE_STRING, stringProperty);
		assertEquals(VALUE_STRING_COLLECTION, stringCollection);
		assertTrue(Arrays.equals(VALUE_STRINGS, stringProperties));
		assertEquals(VALUE_URI, uriProperty);
		assertEquals(VALUE_URI_COLLECTION, uriCollection);
		assertTrue(Arrays.equals(VALUE_URIS, uriProperties));

		final Set<Map.Entry<QName, Object>> extendedProperties = test.getExtendedProperties().entrySet();
		assertEquals(CUSTOM_FIELDS.size(), extendedProperties.size());
		for (final Entry<QName, Object> entry : extendedProperties)
		{
			final Object expected = CUSTOM_FIELDS.get(entry.getKey());
			final Object actual = entry.getValue();
			verifyCustomField(expected, actual);
		}
		
		if (recurse)
		{
			final OslcRestClient aboutOSLCRestClient = new OslcRestClient(PROVIDERS,
																		  aboutURI,
																		  mediaType);

			verifyTest(mediaType,
					   aboutOSLCRestClient.getOslcResource(test.getClass()),
					   false);
		}
	}

	protected static boolean collectionHasResource(final IResource toFind,
												   final Collection<?> c)
	{
		for (final Object next : c)
		{
			if (next instanceof IResource)
			{
				final IResource nextResource = (IResource) next;
				if (toFind.getAbout().equals(nextResource.getAbout()))
				{
					return true;
				}
			}
		}
		
		return false;
	}
	
	protected static void verifyCustomField(final Object expected,
											final Object actual)
	{
		if (expected instanceof Collection && actual instanceof Collection)
		{
			final Collection<?> expectedCollection = (Collection<?>) expected;
			final Collection<?> actualCollection = (Collection<?>) actual;
			assertEquals(expectedCollection.size(), actualCollection.size());
			for (final Object o : actualCollection)
			{
				if (o instanceof IResource)
				{
					assertTrue(collectionHasResource((IResource) o, expectedCollection));
				}
				else
				{
					assertTrue(expectedCollection.contains(o));
				}
			}
		}
		// If the resource didn't have any properties, you'll get back only
		// a URI when reading it in again.
		else if (expected instanceof IResource && actual instanceof URI)
		{
			final IResource expectedResource = (IResource) expected;
			assertEquals(expectedResource.getAbout(), actual);
		}
		else if (expected instanceof Nested && actual instanceof AnyResource)
		{
			final Nested expectedNested = (Nested) expected;
			final AnyResource actualNested = (AnyResource) actual;
			final String expectedString = expectedNested.getStringProperty();
			if (expectedString != null)
			{
				final String actualString = (String) actualNested.getExtendedProperties().get(new QName(Constants.TEST_DOMAIN, "stringProperty"));
				assertEquals(expectedString, actualString);
				
				Collection<URI> actualTypes = actualNested.getTypes();
				try {
					assert(actualTypes.contains(new URI(Constants.TYPE_NESTED)));
				} catch (URISyntaxException e) {
					fail(e.getMessage());
				}
			}
		}
		else if (expected instanceof AnyResource && actual instanceof AnyResource)
		{
			final AnyResource expectedAny = (AnyResource) expected;
			final AnyResource actualAny	  = (AnyResource) actual;
			assertEquals(expectedAny.getTypes(), actualAny.getTypes());
			assertEquals(expectedAny.getExtendedProperties(), actualAny.getExtendedProperties());
		}
		else if (expected instanceof Number && actual instanceof Number)
		{
			// In some cases, the number type might be different when serialized
			// out and then read back in (for instance, a BigInteger might
			// become an Integer). Do our best to compare the values.
			// This loses precision, but should be sufficient for these tests.
			final Number expectedNumber = (Number) expected;
			final Number actualNumber = (Number) actual;
			assertEquals(expectedNumber.doubleValue(), actualNumber.doubleValue()); 
		}
		else if (expected instanceof XMLLiteral && actual instanceof String)
		{
			// Unfortunately, the OSLC 2.0 JSON format does not support XML
			// literals, so the type gets lost when serialized and read back in
			// as a custom property. There is nothing we can do here, so accept
			// string values back.
			final XMLLiteral expectedXmlLiteral = (XMLLiteral) expected;
			assertEquals(expectedXmlLiteral.getValue(), actual);
		}
		else
		{
			assertEquals(expected, actual);
		}
	}
	
	protected static void verifyCompact(final String  mediaType,
										final Compact compact,
										final boolean recurse)
	{
		assertNotNull(compact);

		final URI	  about		   = compact.getAbout();
		final URI	  icon		   = compact.getIcon();
		final Preview largePreview = compact.getLargePreview();
		final String  shortTitle   = compact.getShortTitle();
		final Preview smallPreview = compact.getSmallPreview();
		final String  title		   = compact.getTitle();

		assertNotNull(about);
		assertNotNull(icon);
		assertNotNull(largePreview);
		assertNotNull(shortTitle);
		assertNull(smallPreview);
		assertNotNull(title);

		final URI	 document	   = largePreview.getDocument();
		final String hintHeight	   = largePreview.getHintHeight();
		final String hintWidth	   = largePreview.getHintWidth();
		final String initialHeight = largePreview.getInitialHeight();

		assertNotNull(document);
		assertNotNull(hintHeight);
		assertNotNull(hintWidth);
		assertNotNull(initialHeight);

		if (recurse)
		{
			final OslcRestClient aboutOSLCRestClient = new OslcRestClient(PROVIDERS,
																		  about,
																		  mediaType);

			verifyCompact(mediaType,
						  aboutOSLCRestClient.getOslcResource(Compact.class),
						  false);
		}
	}
}