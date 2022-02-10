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
package org.eclipse.lyo.oslc4j.core;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.UriBuilder;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;
import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.datatypes.TypeMapper;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.datatypes.xsd.impl.XMLLiteralType;
import org.apache.jena.ext.com.google.common.base.Strings;
import org.apache.jena.rdf.model.Property;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class OSLC4JUtils {

	private final static Logger log = LoggerFactory.getLogger(OSLC4JUtils.class);

	/**
	 * An OslcWinkApplication should set the value of publicURI to the base URI of the servlet,
	 * which consists of the hostname, (including the port number), plus the servlet ContextPath.
	 * Typical URI would be similar to "localhost:8080/adaptor-webapp".
	 * After setting the publicURI, the servletPath of the application should also be set.
	 * Typical servlet path value would be similar to "/services".
	 * This would result in a servletURI which is a concatenation of the baseURI and its servletPath.
	 * (localhost:8080/adaptor-webapp/services)
	 */
	private static String publicURI = System.getProperty(OSLC4JConstants.OSLC4J_PUBLIC_URI);
	private static String servletPath = null;
	private static String servletURI = null;

	/**
	 * This constant should be set to true for matching the resource rdf:type to
	 * the describes parameter of the OslcResourceShape annotation. By default
	 * this is set to false. This is part of the fix for defect 412755.
	 */
	private static String useBeanClassForParsing = System.getProperty(OSLC4JConstants.OSLC4J_USE_BEAN_CLASS_FOR_PARSING);

	/**
	 * This constant should be set to true when the property type is not
	 * explicitly passed and it should be inferred from the resource shape. By
	 * default this is set to false. This is part of the the fix for defect
	 * 412789.
	 */
	private static String inferTypeFromShape = System.getProperty(OSLC4JConstants.OSLC4J_INFER_TYPE_FROM_SHAPE);

	/**
	 * List of available ResourceShapes. This list will be used to infer the
	 * property type from the resource shape and it will only be considered if
	 * the property inferTypeFromShape is set to true. This is part of the the
	 * fix for defect 412789.
	 */
	private static List<ResourceShape> shapes = new ArrayList<>();

	public static boolean useStrictDatatypes() {
		return parseBooleanPropertyOrDefault(OSLC4JConstants.OSLC4J_STRICT_DATATYPES, true);
	}

	public static boolean alwaysAbbrevXML() {
		return parseBooleanPropertyOrDefault(OSLC4JConstants.OSLC4J_ALWAYS_XML_ABBREV, false);
	}

	/**
	 * Returns the boolean value of org.eclipse.lyo.oslc4j.disableRelativeURIs Default is true if
	 * not set or invalid (relative URIs will not be allowed)
	 *
	 * @return true if relative URIs are disabled, or if the property is not set or invalid.
	 */
	public static boolean relativeURIsAreDisabled() {
		return parseBooleanPropertyOrDefault(OSLC4JConstants.OSLC4J_DISABLE_RELATIVE_URIS, true);
	}

	/**
	 * This method receives the property name and the property value and tries to infer the property
	 * type from the pre-defined list of Resource Shapes. Then returns the corresponding java object
	 * for the given object value. Returns a null object when it was not possible to infer the
	 * property type from the list of Resource Shapes.
	 *
	 * @param propertyQName Property information
	 * @param originalValue Property value
	 *
	 * @return Java object related to the Resource Shape type.
	 *
	 * @throws DatatypeConfigurationException , IllegalArgumentException, InstantiationException,
	 *                                        InvocationTargetException on
	 */
	public static Object getValueBasedOnResourceShapeType(final HashSet<String> rdfTypesList,
			final QName propertyQName, final Object originalValue)
			throws DatatypeConfigurationException, IllegalArgumentException, InstantiationException {
		if (null == rdfTypesList || rdfTypesList.isEmpty() || null == propertyQName || null == originalValue) {
			return null;
		}
		try {
			// get the pre-defined list of ResourceShapes
			List<ResourceShape> shapes = OSLC4JUtils.getShapes();

			if (null == shapes || shapes.isEmpty()) {
				return null;
			}

			// try to find the attribute type in the list of
			// resource shapes
			String propertyName = propertyQName.getNamespaceURI() + propertyQName.getLocalPart();

			TypeMapper typeMapper = TypeMapper.getInstance();

			for (ResourceShape shape : shapes) {

				// ensure that the current resource shape matches the resource rdf:type
				if (!doesResourceShapeMatchRdfTypes(shape, rdfTypesList)) {
					continue;
				}

				org.eclipse.lyo.oslc4j.core.model.Property[] props = shape.getProperties();

				for (org.eclipse.lyo.oslc4j.core.model.Property prop : props) {
					URI propDefinition = prop.getPropertyDefinition();

					if (!propertyName.equals(propDefinition.toString())) {
						continue;
					}
					URI propValueType = prop.getValueType();

					if (null == propValueType) {
						continue;
					}

					RDFDatatype dataTypeFromShape = typeMapper.getTypeByName(propValueType.toString());

					// this is a literal type
					if (null == dataTypeFromShape) {
						continue;
					}
					try {

						// special treatment for XMLLiteral
						if (isXmlLiteralProperty(propValueType)) {
							return xmlLiteralPropertyFrom(originalValue);
						}

						// special treatment for Date
						if (isDateProperty(dataTypeFromShape)) {
							return datePropertyFrom(originalValue);
						}

						// special treatment for Boolean
						if (isBooleanProperty(dataTypeFromShape)) {
							return booleanPropertyFrom(originalValue);

						}

						// special treatment for double
						if (isDoubleProperty(dataTypeFromShape)) {
							return doublePropertyFrom(originalValue);
						}

						// special treatment for float
						if (isFloatProperty(dataTypeFromShape)) {
							return floatPropertyFrom(originalValue);
						}
						Constructor<?> cons = dataTypeFromShape.getJavaClass()
															   .getConstructor(String.class);
						return cons.newInstance(originalValue.toString());
					} catch (IllegalArgumentException | InvocationTargetException | DatatypeFormatException e) {
						throw new IllegalArgumentException(e);
					}
				}
			}
		} catch (NoSuchMethodException | IllegalAccessException e) {
			// if there is any error while creating the new object, return null,
			// i.e use the original value and not the new one.
			// TODO Andrew@2017-07-18: Throw exception instead of returning null
			log.warn("Could not create extended value <{}> based on shape", propertyQName, e);
			return null;
		}

		return null;
	}

	/**
	 * Returns the value of org.eclipse.lyo.oslc4j.publicURI or null if not set.
	 *
	 *
	 * @return Public URI, or null if not set
	 */
	public static String getPublicURI()
	{
		return publicURI;
	}

	/**
	 * Sets the value of org.eclipse.lyo.oslc4j.publicURI
	 * @param newPublicURI
	 * 		The new public URI to use.
	 */
//	 @SuppressWarnings("unused")
	public static void setPublicURI(final String newPublicURI) throws MalformedURLException
	{

		if (newPublicURI != null && !newPublicURI.isEmpty())
		{
			//test for valid URL - exception will be thrown if invalid
			new URL(newPublicURI);
		}
		publicURI = newPublicURI;
	}

	/**
	 * Return the servlet path value.
	 *
	 * @return Servlet Path, or its default "services/" if not set.
	 */
	public static String getServletPath()
	{
		return servletPath;
	}

	/**
	 * Resolve a URI (usually a resource subject or info URI) based on the settings of
	 * org.eclipse.lyo.oslc4j.publicURI and org.eclipse.lyo.oslc4j.disableHostResolution.
	 *
	 * If the publicURI property is set, it takes precedence and is used to build the full URI.
	 *
	 * If the disableHostResolution property is false or not set, resolution of the local hostname
	 * is attempted.
	 *
	 * If the disableHostResolution property is true or resolution has failed, the hostname is
	 * retrieved from the request.
	 *
	 * Query parameters from the request are not copied to the resolved URI.
	 *
	 * @param request - request to base resolved URI on
	 * @param includePath - if the path (after the context root) should be included in the
	 * resolved URI
	 * @return String containing the resolved URI
	 * @deprecated Use {@link #resolveFullUri(HttpServletRequest)} instead.
	 */
	@Deprecated
	public static String resolveURI(HttpServletRequest request, boolean includePath) {
		UriBuilder builder;

		final String pathInfo = request.getPathInfo();
		final String servletPath = request.getServletPath();
		final String configuredPublicURI = getPublicURI();

		if (configuredPublicURI != null && !configuredPublicURI.isEmpty()) {
			//public URI configured, use it - it includes the context
			String uriToBuild = configuredPublicURI;
			if (includePath) {
				uriToBuild = configuredPublicURI + "/" + servletPath + pathInfo;
			}
			builder = UriBuilder.fromUri(uriToBuild); //Normalize later
		} else {
			final String hostname = guessHostname(request);

			String contextPath = request.getContextPath();
			String pathToBuild = contextPath;
			if (includePath) {
				pathToBuild = contextPath + servletPath + pathInfo;
			}
			builder = UriBuilder.fromPath(pathToBuild)
								.scheme(request.getScheme())
								.host(hostname)
								.port(request.getServerPort());
		}

		URI resolvedURI = builder.build().normalize();
		return resolvedURI.toString();
	}

	/**
	 * Return the public URI including the servlet path. Typically, this would
	 * be something like "localhost:8080/adaptor-webapp/services",
	 * whereas the publicURI would typically be the base "localhost:8080/adaptor-webapp"
	 */
	public static String getServletURI()
	{
		return servletURI;
	}

	public static boolean useBeanClassForParsing() {
		boolean result = false;
		if (null != useBeanClassForParsing) {
			result = Boolean.parseBoolean(useBeanClassForParsing);
		}
		return result;
	}

	public static String getUseBeanClassForParsing() {
		return useBeanClassForParsing;
	}

	/**
	 * Returns a list of Resource Shapes to be used when inferring a property type from the Resource
	 * Shape. This method should only be used when the property inferTypeFromShape is set to true.
	 *
	 * @return List of Resource Shapes
	 */
	public static List<ResourceShape> getShapes() {
		return shapes;
	}

    public static boolean inferTypeFromShape() {
		boolean result = false;
		if (null != inferTypeFromShape) {
			result = Boolean.parseBoolean(inferTypeFromShape);
		}
		return result;
	}

	public static String getInferTypeFromShape() {
		return inferTypeFromShape;
	}

	/**
	 * Sets a list of Resource Shapes to be used when inferring a property type
	 * from the Resource Shape. This method should only be used when the
	 * property inferTypeFromShape is set to true.
	 *
	 * @param shapes
	 *			  List of Resource Shapes
	 */
	public static void setShapes(List<ResourceShape> shapes) {
		OSLC4JUtils.shapes = shapes;
	}

	/**
	 * Returns the boolean value of org.eclipse.lyo.oslc4j.disableHostResolution
	 * Default is false if not set or invalid (hostname resolution will take place)
	 * @return true if host resolution is disabled, false if not set or invalid
	 */
	public static boolean isHostResolutionDisabled() {
		return parseBooleanPropertyOrDefault(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION, false);
	}

	/**
	 * Return if the query result list type will be http://www.w3.org/2000/01/rdf-schema#Container
	 * or there will be no type. Default is no type.
	 */
	public static boolean isQueryResultListAsContainer() {
		return parseBooleanPropertyOrDefault(OSLC4JConstants.OSLC4J_QUERY_RESULT_LIST_AS_CONTAINER,
											 false
		);
	}

	// TODO Andrew@2018-03-03: we have to deprecate this, and have users go via system properties
	public static void setUseBeanClassForParsing(String useBeanClassForParsing) {
		OSLC4JUtils.useBeanClassForParsing = useBeanClassForParsing;
	}

	public static void setHostResolutionDisabled(boolean hostResDisabled)
	{
		System.setProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION, Boolean.toString(hostResDisabled));
	}

	/**
	 * Resolve a full request URI (usually a resource subject or info URI) based on the settings of
	 * org.eclipse.lyo.oslc4j.publicURI and org.eclipse.lyo.oslc4j.disableHostResolution.
	 *
	 * If the publicURI property is set, it takes precedence and is used to build the full URI.
	 *
	 * If the servletPath property is set, it takes precedence and is used to build the full URI.
	 *
	 * If the disableHostResolution property is false or not set, resolution of the local hostname
	 * is attempted.
	 *
	 * If the disableHostResolution property is true or resolution has failed, the hostname is
	 * retrieved from the request.
	 *
	 * Query parameters from the request are not copied to the resolved URI.
	 * @param request - request to base resolved URI on
	 * @return String containing the resolved URI
	 */
	public static String resolveFullUri(HttpServletRequest request) {
		final UriBuilder servletUriBuilder = servletUriBuilderFrom(request);

		final String pathInfo = request.getPathInfo();
		final UriBuilder publicUriBuilder = servletUriBuilder.path(pathInfo);

		URI resolvedURI = publicUriBuilder.build().normalize();
		return resolvedURI.toString();
	}

	/**
	 * Resolve a servlet URI based on the settings of
	 * org.eclipse.lyo.oslc4j.publicURI and org.eclipse.lyo.oslc4j.disableHostResolution.
	 *
	 * If the publicURI property is set, it takes precedence and is used to build the full URI.
	 *
	 * If the servletPath property is set, it takes precedence and is used to build the full URI.
	 *
	 * If the disableHostResolution property is false or not set, resolution of the local hostname
	 * is attempted.
	 *
	 * If the disableHostResolution property is true or resolution has failed, the hostname is
	 * retrieved from the request.
	 *
	 * Query parameters from the request are not copied to the resolved URI.
	 * @param request - request to base resolved URI on
	 * @return String containing the resolved URI
	 */
	public static String resolveServletUri(HttpServletRequest request) {
		final UriBuilder servletUriBuilder = servletUriBuilderFrom(request);

		URI resolvedURI = servletUriBuilder.build().normalize();
		return resolvedURI.toString();
	}

	/**
	 * Sets the value of the servlet path.
	 *
	 * This method also sets the resulting value of the servletURI (combining the publicURI with
	 * the servlet path). This requires publicURI to be configured beforehand.
	 *
	 * @param newServletPath New servlet path to set or NULL to reset it.
	 */
	public static void setServletPath(String newServletPath) {
		if (newServletPath != null && !newServletPath.isEmpty()) {
			//test for valid URL - exception will be thrown if invalid
			URI testServletURI = servletUriBuilderFrom(getPublicURI(), newServletPath).build();
			servletPath = newServletPath;
			servletURI = testServletURI.toString();
		} else {
			servletPath = null;
			servletURI = null;
		}
	}

	private static UriBuilder servletUriBuilderFrom(final HttpServletRequest request) {
		final String publicUri = getOrConstructPublicUriBase(request);
		final String servletPath;
		if (getServletPath() != null) {
			servletPath = getServletPath();
		} else {
			servletPath = request.getServletPath();
		}

		return servletUriBuilderFrom(publicUri, servletPath);
	}

	private static UriBuilder servletUriBuilderFrom(final String publicUri,
			final String servletPath) {
		return UriBuilder.fromUri(publicUri).path(servletPath);
	}

	private static String getOrConstructPublicUriBase(final HttpServletRequest request) {
		String publicUri = getPublicURI();
		if (publicUri == null || publicUri.isEmpty()) {
			final String scheme = request.getScheme();
			final String hostName = guessHostname(request);
			final int serverPort = request.getServerPort();
			final String contextPath = request.getContextPath();
			publicUri = constructPublicUriBase(scheme, hostName, serverPort, contextPath);
		}
		return publicUri;
	}

	private static String constructPublicUriBase(final String scheme, final String hostName,
			final int serverPort, final String contextPath) {
		return UriBuilder.fromPath(contextPath)
						 .scheme(scheme)
						 .host(hostName)
						 .port(serverPort)
						 .build()
						 .normalize()
						 .toString();
	}

	// TODO Andrew@2017-07-18: Avoid guessing anything and prefer configuration and/or convention
	@Deprecated
	private static String guessHostname(final HttpServletRequest request) {
		String hostName = "localhost";

		//try host resolution first if property to disable it is false or not set
		boolean getHostNameFromRequest = false;

		if (isHostResolutionDisabled()) {
			getHostNameFromRequest = true;
		} else {
			try {
				hostName = InetAddress.getLocalHost().getCanonicalHostName();
			} catch (UnknownHostException e) {
				//fallback is to use the hostname from request
				log.info("Unable to resolve hostname. Extracting hostname from request.");
				getHostNameFromRequest = true;
			}
		}

		if (getHostNameFromRequest) {
			hostName = request.getServerName();
		}
		return hostName;
	}

	// TODO Andrew@2018-03-03: we have to deprecate this, and have users go via system properties
	public static void setInferTypeFromShape(String inferTypeFromShape) {
		OSLC4JUtils.inferTypeFromShape = inferTypeFromShape;
	}

	/**
	 * @param key          key to get a system property
	 * @param defaultValue used only if the property is missing
	 *
	 * @return boolean value of a property, the default value if it's missing or an
	 *         IllegalArgumentException if the property value is malformed
	 */
	private static Boolean parseBooleanPropertyOrDefault(final String key,
			final boolean defaultValue) {
		Boolean value = null;
		final String property = System.getProperty(key);
		if (Strings.isNullOrEmpty(property)) {
			value = defaultValue;
		} else {
			try {
				value = parseBooleanStrict(property.trim());
			} catch (IllegalArgumentException e) {
				log.error(
						"System property '{}' holds illegal value: '{}' (only 'true' or 'false' are allowed)",
						key,
						property
				);
				throw e;
			}
		}
		return value;
	}

	/**
	 * This method returns true if the given Resource Shape describes array
	 * matches the list of RDF types.
	 *
	 * @param shape
	 *			  Resource Shape
	 * @param rdfTypesList
	 *			  List of rdf:types
	 *
	 * @return True if the ResourceShape type is in the list of rdf:types,
	 *		   otherwise returns false.
	 */
	private static boolean doesResourceShapeMatchRdfTypes(final ResourceShape shape,
			final HashSet<String> rdfTypesList)
	{
		if (null != shape)
		{
			final URI[] describes = shape.getDescribes();
			for (URI describeUri : describes)
			{
				final String describeUriStr = describeUri.toASCIIString();
				if (rdfTypesList.contains(describeUriStr))
				{
					return true;
				}
			}
		}

		return false;
	}

	/**
	 * Parse a boolean more strictly than Java standard library.
	 *
	 * @throws IllegalArgumentException if the value cannot be parsed into true or false
	 */
	private static boolean parseBooleanStrict(final String input) throws IllegalArgumentException {
		if ("true".equalsIgnoreCase(input)) {
			return true;
		} else if ("false".equalsIgnoreCase(input)) {
			return false;
		} else {
			throw new IllegalArgumentException("Value has to be either true or false");
		}
	}

	private static Object floatPropertyFrom(final Object originalValue) {
		return XSDDatatype.XSDfloat.parseValidated(originalValue.toString());
	}

	private static Object doublePropertyFrom(final Object originalValue) {
		return XSDDatatype.XSDdouble.parseValidated(originalValue.toString());
	}

	private static Object booleanPropertyFrom(final Object originalValue) {
		// XML supports both 'true' and '1' for a true
		// Boolean.
		// Cannot use Boolean.parseBoolean since it
		// supports case-insensitive TRUE.
		if ((Boolean.TRUE.toString().equals(
				originalValue.toString())) || ("1".equals(
				originalValue.toString()))) {
			return Boolean.TRUE;
		}
		// XML supports both 'false' and '0' for a false
		// Boolean.
		else if ((Boolean.FALSE.toString().equals(
				originalValue.toString())) || ("0".equals(
				originalValue.toString()))) {
			return Boolean.FALSE;
		} else {
			throw new IllegalArgumentException(
					"'" + originalValue.toString() + "' " + "has " + "wrong " +
							"format for Boolean" + ".");
		}
	}

	private static Object datePropertyFrom(final Object originalValue)
			throws DatatypeConfigurationException {
		String dateStr = originalValue.toString();
		Calendar calendar;
		calendar = DatatypeFactory.newInstance()
								  .newXMLGregorianCalendar(dateStr)
								  .toGregorianCalendar();
		final XSDDateTime xsdDateTime = new XSDDateTime(calendar);
		return xsdDateTime.asCalendar().getTime();
	}

	private static XMLLiteral xmlLiteralPropertyFrom(final Object originalValue) {
		return new XMLLiteral(originalValue.toString());
	}

	private static boolean isFloatProperty(final RDFDatatype dataTypeFromShape) {
		return dataTypeFromShape.getJavaClass().getCanonicalName().equals(
				Float.class.getCanonicalName());
	}

	private static boolean isDoubleProperty(final RDFDatatype dataTypeFromShape) {
		return dataTypeFromShape.getJavaClass().getCanonicalName().equals(
				Double.class.getCanonicalName());
	}

	private static boolean isBooleanProperty(final RDFDatatype dataTypeFromShape) {
		return dataTypeFromShape.getJavaClass().getCanonicalName().equals(
				Boolean.class.getCanonicalName());
	}

	private static boolean isDateProperty(final RDFDatatype dataTypeFromShape) {
		return dataTypeFromShape.getJavaClass().getCanonicalName().equals(
				XSDDateTime.class.getCanonicalName());
	}

	private static boolean isXmlLiteralProperty(final URI propValueType) {
		return XMLLiteralType.theXMLLiteralType.getURI().equals(propValueType.toString());
	}

	/**
	 * This method receives the property name and the property value and tries
	 * to infer the property Data Type from the pre-defined list of Resource Shapes.
	 * Returns a null object when it was not possible to infer the property Data Type
	 * from the list of Resource Shapes.
	 *
	 * @param rdfTypesList
	 * 		List of rdf:types
	 * @param property
	 *			  Property information
	 * @return Java object related to the Resource Shape type.
	 * @throws DatatypeConfigurationException
	 *			   , IllegalArgumentException, InstantiationException,
	 *			   InvocationTargetException
	 *
	 */
	public static RDFDatatype getDataTypeBasedOnResourceShapeType(final HashSet<String>
			rdfTypesList,
			final Property property )
	{
		if (null != rdfTypesList && !rdfTypesList.isEmpty() && null != property )
		{
			try {
				// get the pre-defined list of ResourceShapes
				List<ResourceShape> shapes = OSLC4JUtils.getShapes();

				if (null != shapes && !shapes.isEmpty()) {

					// try to find the attribute type in the list of
					// resource shapes
					String propertyName = property.getURI();

					TypeMapper typeMapper = TypeMapper.getInstance();

					for (ResourceShape shape : shapes) {

						// ensure that the current resource shape matches the resource rdf:type
						if (doesResourceShapeMatchRdfTypes(shape, rdfTypesList)) {

							org.eclipse.lyo.oslc4j.core.model.Property[] props = shape
									.getProperties();

							for (org.eclipse.lyo.oslc4j.core.model.Property prop : props) {
								URI propDefinition = prop.getPropertyDefinition();

								if (propertyName.equals(propDefinition.toString())) {
									URI propValueType = prop.getValueType();

									if (null == propValueType) {
										continue;
									}
									return typeMapper.getTypeByName(propValueType.toString());
								}
							}
						}
					}
				}
			} catch (Exception e) {
				// if there is any error, return null
				// TODO Andrew@2017-07-18: Throw an exception instead of throwing null
				log.warn("Could not find Data Type <{}> based on shape", property, e);
				return null;
			}
		}
		return null;
	}
	
	 /**
     * @see OSLC4JConstants#LYO_STORE_PAGING_UNSAFE
     * @return the boolean value of org.eclipse.lyo.oslc4j.unsafePaging
     * Default is false if not set.
     */
    public static boolean isLyoStorePagingUnsafe() {
        return parseBooleanPropertyOrDefault(OSLC4JConstants.LYO_STORE_PAGING_UNSAFE, false);
    }

    public static void setLyoStorePagingUnsafe(boolean value) {
        System.setProperty(OSLC4JConstants.LYO_STORE_PAGING_UNSAFE, Boolean.toString(value));
    }

	 /**
     * @see OSLC4JConstants#LYO_STORE_PAGING_UNSAFE
     * @return the boolean value of org.eclipse.lyo.oslc4j.unsafePaging
     * Default is true if not set.
     */
    public static boolean hasLyoStorePagingPreciseLimit() {
        return parseBooleanPropertyOrDefault(OSLC4JConstants.LYO_STORE_PAGING_PRECISE_LIMIT, true);
    }

    public static void setLyoStorePagingPreciseLimit(boolean value) {
        System.setProperty(OSLC4JConstants.LYO_STORE_PAGING_PRECISE_LIMIT, Boolean.toString(value));
    }

}
