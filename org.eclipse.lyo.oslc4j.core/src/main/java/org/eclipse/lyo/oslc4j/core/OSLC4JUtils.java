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
 *	   Michael Fiedler		 - initial API and implementation
 *	   
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core;

import com.hp.hpl.jena.datatypes.DatatypeFormatException;
import com.hp.hpl.jena.datatypes.RDFDatatype;
import com.hp.hpl.jena.datatypes.TypeMapper;
import com.hp.hpl.jena.datatypes.xsd.XSDDatatype;
import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.datatypes.xsd.impl.XMLLiteralType;
import com.hp.hpl.jena.rdf.model.Property;
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
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.UriBuilder;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;


public class OSLC4JUtils {

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
	
	private static final Logger logger = Logger.getLogger(OSLC4JUtils.class.getName());
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
	@SuppressWarnings("unused")
	public static void setPublicURI(String newPublicURI) throws MalformedURLException
	{

		if (newPublicURI != null && !newPublicURI.isEmpty())
		{
			//test for valid URL - exception will be thrown if invalid
			URL newPublicURL = new URL(newPublicURI);
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

	public static void setUseBeanClassForParsing(String useBeanClassForParsing) {
		OSLC4JUtils.useBeanClassForParsing = useBeanClassForParsing;
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

	public static void setInferTypeFromShape(String inferTypeFromShape) {
		OSLC4JUtils.inferTypeFromShape = inferTypeFromShape;
	}

	/**
	 * Returns a list of Resource Shapes to be used when inferring a property
	 * type from the Resource Shape. This method should only be used when the
	 * property inferTypeFromShape is set to true.
	 * 
	 * @return List of Resource Shapes
	 */
	public static List<ResourceShape> getShapes() {
		return shapes;
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
	public static boolean isHostResolutionDisabled()
	{
		boolean retVal = false;
		
		String hostResDisabledProp = System.getProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION);
		if (hostResDisabledProp !=null)
		{
			retVal = Boolean.parseBoolean(hostResDisabledProp);
		}
		return retVal;
		
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
				logger.finer("Unable to resolve hostname.  Extracting hostname from request.");
				getHostNameFromRequest = true;
			}
		}

		if (getHostNameFromRequest) {
			hostName = request.getServerName();
		}
		return hostName;
	}

	/**
	 * Returns the boolean value of org.eclipse.lyo.oslc4j.disableRelativeURIs
	 * Default is true if not set or invalid (relative URIs will not be allowed)
	 * @return true if relative URIs are disabled, or if the property is not set or invalid.
	 */
	public static boolean relativeURIsAreDisabled()
	{
		boolean retVal = true;
		
		String relURIsDisabledProp = System.getProperty(OSLC4JConstants.OSLC4J_DISABLE_RELATIVE_URIS);
		if (relURIsDisabledProp !=null)
		{
			retVal = Boolean.parseBoolean(relURIsDisabledProp);
		}
		return retVal;
	}
	
	/**
	 * Return if the query result list type will be	 
	 * http://www.w3.org/2000/01/rdf-schema#Container or there will be no type.
	 * Default is no type.
	 */
	public static boolean isQueryResultListAsContainer()
	{
		return "true".equals(System.getProperty(OSLC4JConstants.OSLC4J_QUERY_RESULT_LIST_AS_CONTAINER, "false"));
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
	 * This method receives the property name and the property value and tries
	 * to infer the property type from the pre-defined list of Resource Shapes.
	 * Then returns the corresponding java object for the given object value.
	 * Returns a null object when it was not possible to infer the property type
	 * from the list of Resource Shapes.
	 * 
	 * @param rdfTypesList
	 * @param propertyQName
	 *			  Property information
	 * @param originalValue
	 *			  Property value
	 * @return Java object related to the Resource Shape type.
	 * @throws DatatypeConfigurationException
	 *			   , IllegalArgumentException, InstantiationException,
	 *			   InvocationTargetException
	 on 
	 * 
	 */
	public static Object getValueBasedOnResourceShapeType(final HashSet<String> rdfTypesList,
														  final QName propertyQName,
														  final Object originalValue)
			throws DatatypeConfigurationException,
				   IllegalArgumentException,
				   InstantiationException,
				   InvocationTargetException
	{
		if (null != rdfTypesList && !rdfTypesList.isEmpty() && null != propertyQName && null != originalValue)
		{
			try {
				// get the pre-defined list of ResourceShapes
				List<ResourceShape> shapes = OSLC4JUtils.getShapes();

				if (null != shapes && !shapes.isEmpty()) {

					// try to find the attribute type in the list of
					// resource shapes
					String propertyName = propertyQName.getNamespaceURI()
							+ propertyQName.getLocalPart();

					TypeMapper typeMapper = TypeMapper.getInstance();

					for (ResourceShape shape : shapes) {

						// ensure that the current resource shape matches the resource rdf:type
						if (doesResourceShapeMatchRdfTypes(shape, rdfTypesList)) {

							org.eclipse.lyo.oslc4j.core.model.Property[] props = shape.getProperties();

							for (org.eclipse.lyo.oslc4j.core.model.Property prop : props) {
								URI propDefinition = prop.getPropertyDefinition();

								if (propertyName.equals(propDefinition.toString())) {
									URI propValueType = prop.getValueType();

									if (null == propValueType) {
										continue;
									}

									RDFDatatype dataTypeFromShape = typeMapper.getTypeByName(propValueType.toString());

									// this is a literal type
									if (null != dataTypeFromShape) {
										try {

											// special treatment for XMLLiteral
											if (XMLLiteralType.theXMLLiteralType.getURI().equals(propValueType.toString())) {
												return new XMLLiteral(originalValue.toString());
											}
	
											// special treatment for Date
											Class<?> objClass = dataTypeFromShape.getJavaClass();
											if (objClass.getCanonicalName().equals(XSDDateTime.class.getCanonicalName())) {
												String dateStr = originalValue.toString();
												Calendar calendar;
												calendar = DatatypeFactory.newInstance().newXMLGregorianCalendar(dateStr).toGregorianCalendar();
												final XSDDateTime xsdDateTime = new XSDDateTime(calendar);
												return xsdDateTime.asCalendar().getTime();
											}
											
											// special treatment for Boolean
											if (objClass.getCanonicalName().equals(Boolean.class.getCanonicalName())) {
												// XML supports both 'true' and '1' for a true Boolean.
												// Cannot use Boolean.parseBoolean since it supports case-insensitive TRUE.
												if ((Boolean.TRUE.toString().equals(originalValue.toString())) || ("1".equals(originalValue.toString()))) {
												   return Boolean.TRUE;
												}
												// XML supports both 'false' and '0' for a false Boolean.
												else if ((Boolean.FALSE.toString().equals(originalValue.toString())) || ("0".equals(originalValue.toString()))) {
													return Boolean.FALSE;
												}
												else {
													throw new IllegalArgumentException("'" + originalValue.toString() + "' has wrong format for Boolean.");
												}
											}
											
											// special treatment for double
											if (objClass.getCanonicalName().equals(Double.class.getCanonicalName())) {
												return XSDDatatype.XSDdouble.parseValidated(originalValue.toString());
											}
											
											// special treatment for float
											if (objClass.getCanonicalName().equals(Float.class.getCanonicalName())) {
												return XSDDatatype.XSDfloat.parseValidated(originalValue.toString());
											}
											Constructor<?> cons = objClass.getConstructor(String.class);
											return cons.newInstance(originalValue.toString());
										} catch (IllegalArgumentException e) {											
											String errorMessage = (null == e.getMessage()) ? e.getCause().toString() :	e.getMessage();
											throw new IllegalArgumentException(errorMessage, e);
										} catch (InvocationTargetException e) {											
											String errorMessage = (null == e.getMessage()) ? e.getCause().toString() :	e.getMessage();
											throw new IllegalArgumentException(errorMessage, e);
										} catch (DatatypeFormatException e) {											
											String errorMessage = (null == e.getMessage()) ? e.getCause().toString() :	e.getMessage();
											throw new IllegalArgumentException(errorMessage, e);
										}
									}
								}
							}
						}
					}
				}
			} catch (NoSuchMethodException e) {
				// if there is any error while creating the new object, return null,
				// i.e use the original value and not the new one.
				logger.warning("Could not create extended value <" + propertyQName + " +> based on shape: " + e.getLocalizedMessage());
				return null;
			} catch (IllegalAccessException e) {
				// if there is any error while creating the new object, return null,
				// i.e use the original value and not the new one.
				// if there is any error while creating the new object, return null,
				// i.e use the original value and not the new one.
				logger.warning("Could not create extended value <" + propertyQName + " +> based on shape: " + e.getLocalizedMessage());
				return null;
			}
		}

		return null;
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
	public static RDFDatatype getDataTypeBasedOnResourceShapeType(final HashSet<String> rdfTypesList,
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

							org.eclipse.lyo.oslc4j.core.model.Property[] props = shape.getProperties();

							for (org.eclipse.lyo.oslc4j.core.model.Property prop : props) {
								URI propDefinition = prop.getPropertyDefinition();

								if (propertyName.equals(propDefinition.toString())) {
									URI propValueType = prop.getValueType();

									if (null == propValueType) {
										continue;
									}
									RDFDatatype dataTypeFromShape = typeMapper.getTypeByName(propValueType.toString());
									return dataTypeFromShape;
								}
							}
						}
					}
				}
			} catch (Exception e) {
				// if there is any error, return null,
				logger.warning("Could not find Data Type <" + property + " +> based on shape: " + e.getLocalizedMessage());
				return null;
			}
		}
		return null;
	}
}
