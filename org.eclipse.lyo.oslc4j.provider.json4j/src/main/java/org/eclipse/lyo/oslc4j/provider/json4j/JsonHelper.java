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
package org.eclipse.lyo.oslc4j.provider.json4j;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.AbstractSequentialList;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Deque;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Queue;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;

import org.apache.wink.json4j.JSONArray;
import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.oslc4j.core.NestedWildcardProperties;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.OslcGlobalNamespaceProvider;
import org.eclipse.lyo.oslc4j.core.SingletonWildcardProperties;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespaceDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyTypeException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingNamespaceDeclarationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingNamespacePrefixException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingSetMethodException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreRelativeURIException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.AnyResource;
import org.eclipse.lyo.oslc4j.core.model.IExtendedResource;
import org.eclipse.lyo.oslc4j.core.model.IOslcCustomNamespaceProvider;
import org.eclipse.lyo.oslc4j.core.model.IReifiedResource;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.core.model.InheritedMethodAnnotationHelper;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.TypeFactory;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;

/**
 * Use JSON-LD support in Jena provider.
 */
@Deprecated
public final class JsonHelper
{
	private static final String JSON_PROPERTY_DELIMITER			   = ":";
	private static final String JSON_PROPERTY_PREFIXES			   = "prefixes";
	private static final String JSON_PROPERTY_SUFFIX_ABOUT		   = "about";
	private static final String JSON_PROPERTY_SUFFIX_MEMBER		   = "member";
	private static final String JSON_PROPERTY_SUFFIX_RESOURCE	   = "resource";
	private static final String JSON_PROPERTY_SUFFIX_RESPONSE_INFO = "responseInfo";
	private static final String JSON_PROPERTY_SUFFIX_RESULTS	   = "results";
	private static final String JSON_PROPERTY_SUFFIX_TOTAL_COUNT   = "totalCount";
	private static final String JSON_PROPERTY_SUFFIX_NEXT_PAGE	   = "nextPage";
	private static final String JSON_PROPERTY_SUFFIX_TYPE		   = "type";
	private static final String JSON_PROPERTY_SUFFIX_FIRST		   = "first";
	private static final String JSON_PROPERTY_SUFFIX_REST		   = "rest";
	private static final String JSON_PROPERTY_SUFFIX_NIL		   = "nil";
	private static final String JSON_PROPERTY_SUFFIX_LIST		   = "List";
	private static final String JSON_PROPERTY_SUFFIX_ALT		   = "Alt";
	private static final String JSON_PROPERTY_SUFFIX_BAG		   = "Bag";
	private static final String JSON_PROPERTY_SUFFIX_SEQ		   = "Seq";

	private static final String RDF_ABOUT_URI	 = OslcConstants.RDF_NAMESPACE + JSON_PROPERTY_SUFFIX_ABOUT;
	private static final String RDF_TYPE_URI	 = OslcConstants.RDF_NAMESPACE + JSON_PROPERTY_SUFFIX_TYPE;
	private static final String RDF_NIL_URI		 = OslcConstants.RDF_NAMESPACE + JSON_PROPERTY_SUFFIX_NIL;
	private static final String RDF_RESOURCE_URI = OslcConstants.RDF_NAMESPACE + JSON_PROPERTY_SUFFIX_RESOURCE;

	private static final String METHOD_NAME_START_GET = "get";
	private static final String METHOD_NAME_START_IS  = "is";
	private static final String METHOD_NAME_START_SET = "set";

	private static final int METHOD_NAME_START_GET_LENGTH = METHOD_NAME_START_GET.length();
	private static final int METHOD_NAME_START_IS_LENGTH  = METHOD_NAME_START_IS.length();

	private static final String POSITIVE_INF = "INF";
	private static final String NEGATIVE_INF = "-INF";
	private static final String NOT_A_NUMBER = "NaN";

	/**
	 * System property {@value} : When "true", write "INF", "-INF", and "NaN"
	 * strings for Infinity, -Infinity, and NaN float and double values,
	 * respectively. Enabled by default.
	 *
	 * @see #OSLC4J_READ_SPECIAL_NUMS
	 */
	public static final String OSLC4J_WRITE_SPECIAL_NUMS = "org.eclipse.lyo.oslc4j.writeSpecialNumberValues";

	/**
	 * System property {@value} : When "true", read "INF", "-INF", and "NaN"
	 * strings for Infinity, -Infinity, and NaN float and double values,
	 * respectively. Enabled by default.
	 *
	 * @see #OSLC4J_WRITE_SPECIAL_NUMS
	 */
	public static final String OSLC4J_READ_SPECIAL_NUMS	 = "org.eclipse.lyo.oslc4j.readSpecialNumberValues";

	private static final Logger logger = Logger.getLogger(JsonHelper.class.getName());

	private JsonHelper()
	{
		super();
	}

	public static JSONObject createJSON(final String			  descriptionAbout,
										final String			  responseInfoAbout,
										final ResponseInfo<?>		  responseInfo,
										final Object[]			  objects,
										final Map<String, Object> properties)
		   throws DatatypeConfigurationException,
				  IllegalAccessException,
				  IllegalArgumentException,
				  InvocationTargetException,
				  JSONException,
				  OslcCoreApplicationException
	{
		final JSONObject resultJSONObject = new JSONObject();

		final Map<String, String> namespaceMappings		   = new TreeMap<String, String>();
		final Map<String, String> reverseNamespaceMappings = new HashMap<String, String>();

		// Add all global namespace mappings, since they have lower precedence
		Map<String, String> globalPrefixDefinitionMap = OslcGlobalNamespaceProvider.getInstance().getPrefixDefinitionMap();
		for(Map.Entry<String, String> prefixDefinitionEntry : globalPrefixDefinitionMap.entrySet()) {
			namespaceMappings.put(prefixDefinitionEntry.getKey(), prefixDefinitionEntry.getValue());
			reverseNamespaceMappings.put(prefixDefinitionEntry.getValue(), prefixDefinitionEntry.getKey());
		}

		if (descriptionAbout != null)
		{
			final JSONArray jsonArray = new JSONArray();

			for (final Object object : objects)
			{
				HashMap<Object,JSONObject> visitedObjects = new HashMap<Object,JSONObject>();
				final JSONObject jsonObject = handleSingleResource(object,
																   new JSONObject(),
																   namespaceMappings,
																   reverseNamespaceMappings,
																   properties,
																   visitedObjects);

				if (jsonObject != null)
				{
					jsonArray.add(jsonObject);
				}
			}

			// Ensure we have an rdf prefix
			final String rdfPrefix = ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
														   OslcConstants.RDF_NAMESPACE,
														   namespaceMappings,
														   reverseNamespaceMappings);

			// Ensure we have an rdfs prefix
			final String rdfsPrefix = ensureNamespacePrefix(OslcConstants.RDFS_NAMESPACE_PREFIX,
															OslcConstants.RDFS_NAMESPACE,
															namespaceMappings,
															reverseNamespaceMappings);

			resultJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_ABOUT,
								 descriptionAbout);

			/* Support for Container rdf:type */
			if(OSLC4JUtils.isQueryResultListAsContainer()){
				final JSONArray containerTypesJSONArray = new JSONArray();

				final JSONObject containerTypeJSONObject = new JSONObject();


				containerTypeJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
						OslcConstants.TYPE_CONTAINER);


				containerTypesJSONArray.add(containerTypeJSONObject);

				resultJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TYPE,
								 containerTypesJSONArray);

				Map<Object,JSONObject> visitedObjects = new HashMap<Object,JSONObject>();
				addExtendedProperties(namespaceMappings,
									  reverseNamespaceMappings,
									  resultJSONObject,
									  (IExtendedResource) responseInfo.getContainer(),
									  properties,
									  visitedObjects);
			}


			resultJSONObject.put(rdfsPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_MEMBER,
								 jsonArray);

			if (responseInfoAbout != null)
			{
				// Ensure we have an oslc prefix
				final String oslcPrefix = ensureNamespacePrefix(OslcConstants.OSLC_CORE_NAMESPACE_PREFIX,
																OslcConstants.OSLC_CORE_NAMESPACE,
																namespaceMappings,
																reverseNamespaceMappings);

				final JSONObject responseInfoJSONObject = new JSONObject();

				responseInfoJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_ABOUT,
										   responseInfoAbout);



				if (responseInfo != null)
				{
					responseInfoJSONObject.put(oslcPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TOTAL_COUNT,
							responseInfo.totalCount() == null ? objects.length : responseInfo.totalCount());

					if (responseInfo.nextPage() != null)
					{
						final JSONObject nextPageJSONObject = new JSONObject();
						nextPageJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
								responseInfo.nextPage());
						responseInfoJSONObject.put(oslcPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_NEXT_PAGE,
								nextPageJSONObject);
					}

					final JSONArray responseInfoTypesJSONArray = new JSONArray();

					final JSONObject responseInfoTypeJSONObject = new JSONObject();

					responseInfoTypeJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
												OslcConstants.TYPE_RESPONSE_INFO);

					responseInfoTypesJSONArray.add(responseInfoTypeJSONObject);

					responseInfoJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TYPE,
											responseInfoTypesJSONArray);

					resultJSONObject.put(oslcPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESPONSE_INFO,
										responseInfoJSONObject);

					Map<Object,JSONObject> visitedObjects = new HashMap<Object,JSONObject>();
					addExtendedProperties(namespaceMappings,
										  reverseNamespaceMappings,
										  responseInfoJSONObject,
										  (IExtendedResource) responseInfo,
										  properties,
										  visitedObjects);
				}
			}
		}
		else if (objects.length == 1)
		{
			HashMap<Object,JSONObject> visitedObjects = new HashMap<Object,JSONObject>();
			handleSingleResource(objects[0],
								 resultJSONObject,
								 namespaceMappings,
								 reverseNamespaceMappings,
								 properties,
								 visitedObjects);
		}

		// Set the namespace prefixes
		final JSONObject namespaces = new JSONObject();
		for (final Map.Entry<String, String> namespaceMapping : namespaceMappings.entrySet())
		{
			namespaces.put(namespaceMapping.getKey(),
						   namespaceMapping.getValue());
		}

		if (namespaces.size() > 0)
		{
			resultJSONObject.put(JSON_PROPERTY_PREFIXES,
								 namespaces);
		}

		return resultJSONObject;
	}

	public static Object[] fromJSON(final JSONObject jsonObject,
									final Class<?>	 beanClass)
		   throws DatatypeConfigurationException,
				  IllegalAccessException,
				  IllegalArgumentException,
				  InstantiationException,
				  InvocationTargetException,
				  OslcCoreApplicationException,
				  URISyntaxException
	{
		final List<Object>		  beans					   = new ArrayList<Object>();
		final Map<String, String> namespaceMappings		   = new HashMap<String, String>();
		final Map<String, String> reverseNamespaceMappings = new HashMap<String, String>();

		// First read the prefixes and set up maps so we can create full property definition values later
		final Object prefixes = jsonObject.opt(JSON_PROPERTY_PREFIXES);

		if (prefixes instanceof JSONObject)
		{
			final JSONObject prefixesJSONObject = (JSONObject) prefixes;

			@SuppressWarnings({"unchecked", "cast"})
			final Set<Map.Entry<String, Object>> prefixesEntrySet = (Set<Map.Entry<String, Object>>)  prefixesJSONObject.entrySet();
			for (final Map.Entry<String, Object> prefixEntry : prefixesEntrySet)
			{
				final String prefix	   = prefixEntry.getKey();
				final Object namespace = prefixEntry.getValue();

				if (namespace instanceof String)
				{
					namespaceMappings.put(prefix,
										  namespace.toString());

					reverseNamespaceMappings.put(namespace.toString(),
												 prefix.toString());
				}
			}
		}

		// We have to know the reverse mapping for the rdf namespace
		final String rdfPrefix = reverseNamespaceMappings.get(OslcConstants.RDF_NAMESPACE);

		if (rdfPrefix == null)
		{
			throw new OslcCoreMissingNamespaceDeclarationException(OslcConstants.RDF_NAMESPACE);
		}

		final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods = new HashMap<Class<?>, Map<String, Method>>();

		JSONArray jsonArray = null;

		// Look for rdfs:member
		final String rdfsPrefix = reverseNamespaceMappings.get(OslcConstants.RDFS_NAMESPACE);

		if (rdfsPrefix != null)
		{
			final Object members = jsonObject.opt(rdfsPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_MEMBER);

			if (members instanceof JSONArray)
			{
				jsonArray = (JSONArray) members;
			}
		}

		if (jsonArray == null)
		{
			// Look for oslc:results.  Seen in ChangeManagement.
			final String oslcPrefix = reverseNamespaceMappings.get(OslcConstants.OSLC_CORE_NAMESPACE);

			if (oslcPrefix != null)
			{
				final Object results = jsonObject.opt(oslcPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESULTS);

				if (results instanceof JSONArray)
				{
					jsonArray = (JSONArray) results;
				}
			}
		}

		if (jsonArray != null)
		{
			for (final Object object : jsonArray)
			{
				if (object instanceof JSONObject)
				{
					final JSONObject resourceJSONObject = (JSONObject) object;

					if (URI.class.equals(beanClass)) {
						String uri = resourceJSONObject.optString(rdfPrefix + JSON_PROPERTY_DELIMITER
								+ JSON_PROPERTY_SUFFIX_RESOURCE);

						beans.add(URI.create(uri));
					}
					else
					{
						final Object bean = beanClass.newInstance();
						HashSet<String> rdfTypes = new HashSet<String>();

						fromJSON(rdfPrefix,
								 namespaceMappings,
								 classPropertyDefinitionsToSetMethods,
								 resourceJSONObject,
								 beanClass,
								 bean,
								 rdfTypes);

						beans.add(bean);
					}
				}
			}
		}
		else
		{
			final Object bean = beanClass.newInstance();
			HashSet<String> rdfTypes = new HashSet<String>();

			fromJSON(rdfPrefix,
					 namespaceMappings,
					 classPropertyDefinitionsToSetMethods,
					 jsonObject,
					 beanClass,
					 bean,
					 rdfTypes);

			beans.add(bean);
		}

		return beans.toArray((Object[]) Array.newInstance(beanClass,
														  beans.size()));
	}

	private static void buildAttributeResource(final Map<String, String>	namespaceMappings,
											   final Map<String, String>	reverseNamespaceMappings,
											   final Class<?>				resourceClass,
											   final Method					method,
											   final OslcPropertyDefinition propertyDefinitionAnnotation,
											   final JSONObject				jsonObject,
											   final Object					value,
											   final Map<String, Object>	nestedProperties,
											   final boolean				onlyNested)
			throws DatatypeConfigurationException,
				   IllegalAccessException,
				   IllegalArgumentException,
				   InvocationTargetException,
				   JSONException,
				   OslcCoreApplicationException
	{
		final String propertyDefinition = propertyDefinitionAnnotation.value();

		String name;
		final OslcName nameAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method,
																					  OslcName.class);

		if (nameAnnotation != null)
		{
			name = nameAnnotation.value();
		}
		else
		{
			name = getDefaultPropertyName(method);
		}

		if (!propertyDefinition.endsWith(name))
		{
			throw new OslcCoreInvalidPropertyDefinitionException(resourceClass,
																 method,
																 propertyDefinitionAnnotation);
		}

		final boolean isRdfContainer;

		final OslcRdfCollectionType collectionType =
			InheritedMethodAnnotationHelper.getAnnotation(method,
														  OslcRdfCollectionType.class);

		if (collectionType != null &&
				OslcConstants.RDF_NAMESPACE.equals(collectionType.namespaceURI()) &&
					(JSON_PROPERTY_SUFFIX_LIST.equals(collectionType.collectionType())
					 || JSON_PROPERTY_SUFFIX_ALT.equals(collectionType.collectionType())
					 || JSON_PROPERTY_SUFFIX_BAG.equals(collectionType.collectionType())
					 || JSON_PROPERTY_SUFFIX_SEQ.equals(collectionType.collectionType())))
		{
		   isRdfContainer = true;
		}
		else
		{
		   isRdfContainer = false;
		}

		final Object localResourceValue;

		final Class<?> returnType = method.getReturnType();

		if (returnType.isArray())
		{
			final JSONArray jsonArray = new JSONArray();

			// We cannot cast to Object[] in case this is an array of primitives.  We will use Array reflection instead.
			// Strange case about primitive arrays:	 they cannot be cast to Object[], but retrieving their individual elements
			// does not return primitives, but the primitive object wrapping counterparts like Integer, Byte, Double, etc.
			final int length = Array.getLength(value);
			for (int index = 0;
				 index < length;
				 index++)
			{
				final Object object = Array.get(value,
												index);

				final Object localResource = handleLocalResource(namespaceMappings,
																 reverseNamespaceMappings,
																 resourceClass,
																 method,
																 object,
																 nestedProperties,
																 onlyNested);
				if (localResource != null)
				{
					jsonArray.add(localResource);
				}
			}

			if (isRdfContainer)
			{
				localResourceValue = buildContainer(namespaceMappings,
													reverseNamespaceMappings,
													collectionType, jsonArray);
			}
			else
			{
				if (jsonArray.size() > 0)
				{
					localResourceValue = jsonArray;
				}
				else
				{
					localResourceValue = null;
				}
			}
		}
		else if (Collection.class.isAssignableFrom(returnType))
		{
			final JSONArray jsonArray = new JSONArray();

			@SuppressWarnings("unchecked")
			final Collection<Object> collection = (Collection<Object>) value;

			for (final Object object : collection)
			{
				final Object localResource = handleLocalResource(namespaceMappings,
																 reverseNamespaceMappings,
																 resourceClass,
																 method,
																 object,
																 nestedProperties,
																 onlyNested);
				if (localResource != null)
				{
					jsonArray.add(localResource);
				}
			}

			if (isRdfContainer)
			{
				localResourceValue = buildContainer(namespaceMappings,
													reverseNamespaceMappings,
													collectionType, jsonArray);
			}
			else
			{
				if (jsonArray.size() > 0)
				{
					localResourceValue = jsonArray;
				}
				else
				{
					localResourceValue = null;
				}
			}
		}
		else
		{
			localResourceValue = handleLocalResource(namespaceMappings,
													 reverseNamespaceMappings,
													 resourceClass,
													 method,
													 value,
													 nestedProperties,
													 onlyNested);
		}

		if (localResourceValue != null)
		{
			final String namespace = propertyDefinition.substring(0,
																  propertyDefinition.length() - name.length());

			final String prefix = reverseNamespaceMappings.get(namespace);

			if (prefix == null)
			{
				throw new OslcCoreMissingNamespaceDeclarationException(namespace);
			}

			jsonObject.put(prefix + JSON_PROPERTY_DELIMITER + name,
						   localResourceValue);
		}
	}

	private static Object buildContainer(final Map<String, String>	 namespaceMappings,
										 final Map<String, String>	 reverseNamespaceMappings,
										 final OslcRdfCollectionType collectionType,
										 final JSONArray			 jsonArray)
			throws JSONException
	{
		// Ensure we have an rdf prefix
		final String rdfPrefix = ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
													   OslcConstants.RDF_NAMESPACE,
													   namespaceMappings,
													   reverseNamespaceMappings);

		if (JSON_PROPERTY_SUFFIX_LIST.equals(collectionType.collectionType()))
		{
			JSONObject listObject = new JSONObject();

			listObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
						   OslcConstants.RDF_NAMESPACE + JSON_PROPERTY_SUFFIX_NIL);

			for (int i = jsonArray.size() - 1; i >= 0; i --)
			{
				Object o = jsonArray.get(i);

				JSONObject newListObject = new JSONObject();
				newListObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_FIRST, o);
				newListObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_REST, listObject);

				listObject = newListObject;
			}

			return listObject;
		}

		JSONObject container = new JSONObject();

		container.put(rdfPrefix + JSON_PROPERTY_DELIMITER + collectionType.collectionType(),
					  jsonArray);

		return container;
	}

	private static void buildResource(final Map<String, String> namespaceMappings,
									  final Map<String, String> reverseNamespaceMappings,
									  final Object				object,
									  final Class<?>			objectClass,
									  final JSONObject			jsonObject,
									  final Map<String, Object> properties,
									  final Map<Object,JSONObject> visitedObjects)
			throws DatatypeConfigurationException,
				   IllegalAccessException,
				   IllegalArgumentException,
				   InvocationTargetException,
				   JSONException,
				   OslcCoreApplicationException
	{
		visitedObjects.put(object, jsonObject);
		buildResourceAttributes(namespaceMappings,
								reverseNamespaceMappings,
								object,
								objectClass,
								jsonObject,
								properties,
								visitedObjects);

		// For JSON, we have to save array of rdf:type

		// Ensure we have an rdf prefix
		final String rdfPrefix = ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
													   OslcConstants.RDF_NAMESPACE,
													   namespaceMappings,
													   reverseNamespaceMappings);

		if (rdfPrefix != null)
		{
			final JSONArray rdfTypesJSONArray = new JSONArray();

			final String qualifiedName;
			if (objectClass.getAnnotation(OslcResourceShape.class) != null)
			{
				qualifiedName = TypeFactory.getQualifiedName(objectClass);
				if (qualifiedName != null)
				{
					addType(rdfPrefix,
							rdfTypesJSONArray,
							qualifiedName);
				}
			}
			else
			{
				qualifiedName = null;
			}

			if (object instanceof IExtendedResource)
			{
				final IExtendedResource extendedResource = (IExtendedResource) object;
				for (final URI type : extendedResource.getTypes())
				{
					final String typeString = type.toString();
					if (!typeString.equals(qualifiedName))
					{
						addType(rdfPrefix,
								rdfTypesJSONArray,
								typeString);
					}
				}
			}

			jsonObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TYPE,
						   rdfTypesJSONArray);
		}
	}

	private static void addType(final String	rdfPrefix,
								final JSONArray rdfTypesJSONArray,
								final String	typeURI)
		  throws JSONException
	{
		  final JSONObject rdfTypeJSONObject = new JSONObject();
		  rdfTypeJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
								typeURI);
		  rdfTypesJSONArray.add(rdfTypeJSONObject);
	}

	private static void buildResourceAttributes(final Map<String, String> namespaceMappings,
												final Map<String, String> reverseNamespaceMappings,
												final Object			  object,
												final Class<?>			  objectClass,
												final JSONObject		  jsonObject,
												final Map<String, Object> properties,
												final Map<Object,JSONObject> visitedObjects)
			throws IllegalAccessException,
				   InvocationTargetException,
				   DatatypeConfigurationException,
				   JSONException,
				   OslcCoreApplicationException
	{
		if (properties == OSLC4JConstants.OSL4J_PROPERTY_SINGLETON)
		{
			return;
		}

		for (final Method method : objectClass.getMethods())
		{
			if (method.getParameterTypes().length == 0)
			{
				final String methodName = method.getName();
				if (((methodName.startsWith(METHOD_NAME_START_GET)) &&
					 (methodName.length() > METHOD_NAME_START_GET_LENGTH)) ||
					((methodName.startsWith(METHOD_NAME_START_IS)) &&
					 (methodName.length() > METHOD_NAME_START_IS_LENGTH)))
				{
					final OslcPropertyDefinition oslcPropertyDefinitionAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method,
																																  OslcPropertyDefinition.class);

					if (oslcPropertyDefinitionAnnotation != null)
					{
						final Object value = method.invoke(object);

						if (value != null)
						{
							Map<String, Object> nestedProperties = null;
							boolean onlyNested = false;

							if (properties != null)
							{
								@SuppressWarnings("unchecked")
								final Map<String, Object> map = (Map<String, Object>)properties.get(oslcPropertyDefinitionAnnotation.value());

								if (map != null)
								{
									nestedProperties = map;
								}
								else if (properties instanceof SingletonWildcardProperties &&
										 ! (properties instanceof NestedWildcardProperties))
								{
									nestedProperties = OSLC4JConstants.OSL4J_PROPERTY_SINGLETON;
								}
								else if (properties instanceof NestedWildcardProperties)
								{
									nestedProperties = ((NestedWildcardProperties)properties).commonNestedProperties();
									onlyNested = ! (properties instanceof SingletonWildcardProperties);
								}
								else
								{
									continue;
								}
							}

							buildAttributeResource(namespaceMappings,
												   reverseNamespaceMappings,
												   objectClass,
												   method,
												   oslcPropertyDefinitionAnnotation,
												   jsonObject,
												   value,
												   nestedProperties,
												   onlyNested);
						}
					}
				}
			}
		}

		if (object instanceof IExtendedResource)
		{
			final IExtendedResource extendedResource = (IExtendedResource) object;

			addExtendedProperties(namespaceMappings,
								  reverseNamespaceMappings,
								  jsonObject,
								  extendedResource,
								  properties,
								  visitedObjects);
		}
	}

	protected static void addExtendedProperties(final Map<String, String> namespaceMappings,
												final Map<String, String> reverseNamespaceMappings,
												final JSONObject jsonObject,
												final IExtendedResource extendedResource,
												final Map<String, Object> properties,
												final Map<Object, JSONObject> visitedObjects)
			throws JSONException,
				   DatatypeConfigurationException,
				   IllegalAccessException,
				   InvocationTargetException,
				   OslcCoreApplicationException
	{

		// Ensure we have an rdf prefix
		final String rdfPrefix = ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
													   OslcConstants.RDF_NAMESPACE,
													   namespaceMappings,
													   reverseNamespaceMappings);

		String rdfTypeKey = rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TYPE;
		JSONArray typesJSONArray;
		if (jsonObject.containsKey(rdfTypeKey))
		{
			typesJSONArray = (JSONArray) jsonObject.get(rdfTypeKey);
		} else {
			typesJSONArray = new JSONArray();
		}

		final JSONObject typeJSONObject = new JSONObject();

		for (final URI type : extendedResource.getTypes())
		{
			final String propertyName = type.toString();

			if (properties != null &&
				properties.get(propertyName) == null &&
				! (properties instanceof NestedWildcardProperties) &&
				! (properties instanceof SingletonWildcardProperties))
			{
				continue;
			}

			typeJSONObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
					propertyName);

			typesJSONArray.add(typeJSONObject);
		}

		if (typesJSONArray.size() > 0)
		{
			jsonObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TYPE, typesJSONArray);
		}

		for (Map.Entry<QName, Object> extendedProperty : extendedResource.getExtendedProperties().entrySet())
		{
			final String namespace = extendedProperty.getKey().getNamespaceURI();
			final String localName = extendedProperty.getKey().getLocalPart();
			Map<String, Object> nestedProperties = null;
			boolean onlyNested = false;

			if (properties != null)
			{
				@SuppressWarnings("unchecked")
				final Map<String, Object> map = (Map<String, Object>)properties.get(namespace + localName);

				if (map != null)
				{
					nestedProperties = map;
				}
				else if (properties instanceof SingletonWildcardProperties &&
						 ! (properties instanceof NestedWildcardProperties))
				{
					nestedProperties = OSLC4JConstants.OSL4J_PROPERTY_SINGLETON;
				}
				else if (properties instanceof NestedWildcardProperties)
				{
					nestedProperties = ((NestedWildcardProperties)properties).commonNestedProperties();
					onlyNested = ! (properties instanceof SingletonWildcardProperties);
				}
				else
				{
					continue;
				}
			}

			final Object value = getExtendedPropertyJsonValue(namespaceMappings,
															  reverseNamespaceMappings,
															  extendedProperty.getValue(),
															  nestedProperties,
															  onlyNested,
															  visitedObjects);

			if (value == null && ! onlyNested)
			{
				logger.warning("Could not add extended property " + extendedProperty.getKey() + " for resource " + extendedResource.getAbout());
			}
			else
			{
				String prefix = reverseNamespaceMappings.get(namespace);

				if (prefix == null)
				{
					prefix = extendedProperty.getKey().getPrefix();

					// Add the prefix to the JSON namespace mappings.
					namespaceMappings.put(prefix, namespace);
					reverseNamespaceMappings.put(namespace, prefix);
				}

				// Add the value to the JSON object.
				jsonObject.put(prefix + JSON_PROPERTY_DELIMITER + localName, value);
			}
		}
	}

	private static Object getExtendedPropertyJsonValue(final Map<String, String> namespaceMappings,
													   final Map<String, String> reverseNamespaceMappings,
													   final Object				 object,
													   final Map<String, Object> nestedProperties,
													   final boolean			 onlyNested,
													   final Map<Object,JSONObject> visitedObjects)
		throws JSONException,
			   DatatypeConfigurationException,
			   IllegalArgumentException,
			   IllegalAccessException,
			   InvocationTargetException,
			   OslcCoreApplicationException
	{
		final Class<?> resourceClass = object.getClass();
		if (object instanceof Collection)
		{
			final JSONArray jsonArray = new JSONArray();
			@SuppressWarnings("unchecked")
			final Collection<Object> c = (Collection<Object>) object;
			for (final Object next : c)
			{
				final Object nextJson = getExtendedPropertyJsonValue(namespaceMappings,
																	 reverseNamespaceMappings,
																	 next,
																	 nestedProperties,
																	 onlyNested,
																	 visitedObjects);
				if (nextJson != null)
				{
					jsonArray.add(nextJson);
				}
			}

			return jsonArray;
		}
		else if ((object instanceof String)	 ||
				(object instanceof Boolean) ||
				(object instanceof Number))
		{
			if (onlyNested)
			{
				return null;
			}

			return object;
		}
		else if (object instanceof XMLLiteral)
		{
			if (onlyNested)
			{
				return null;
			}

			// XMLLiterals are treated as strings in the OSLC 2.0 JSON format.
			final XMLLiteral xmlLiteral = (XMLLiteral) object;
			return xmlLiteral.getValue();
		}
		else if (object instanceof Date)
		{
			if (onlyNested)
			{
				return null;
			}

			final GregorianCalendar calendar = new GregorianCalendar();
			calendar.setTime((Date) object);

			return DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar).toString();
		}
		else if (object instanceof URI)
		{
			if (onlyNested)
			{
				return null;
			}

			return handleResourceReference(namespaceMappings,
										   reverseNamespaceMappings,
										   resourceClass,
										   null,
										   (URI) object);
		}
		else if (object instanceof IResource && !visitedObjects.containsKey(object))
		{
			return handleSingleResource(object,
										new JSONObject(),
										namespaceMappings,
										reverseNamespaceMappings,
										nestedProperties,
										visitedObjects);
		}
		else if (visitedObjects.containsKey(object))
		{
			JSONObject returnObject = visitedObjects.get(object);
			if (!returnObject.isEmpty())
				return returnObject;
		}

		return null;
	}

	private static String getDefaultPropertyName(final Method method)
	{
		final String methodName	   = method.getName();
		final int	 startingIndex = methodName.startsWith(METHOD_NAME_START_GET) ? METHOD_NAME_START_GET_LENGTH : METHOD_NAME_START_IS_LENGTH;
		final int	 endingIndex   = startingIndex + 1;

		// We want the name to start with a lower-case letter
		final String lowercasedFirstCharacter = methodName.substring(startingIndex,
																	 endingIndex).toLowerCase(Locale.ENGLISH);

		if (methodName.length() == endingIndex)
		{
			return lowercasedFirstCharacter;
		}

		return lowercasedFirstCharacter +
			   methodName.substring(endingIndex);
	}

	private static Object handleLocalResource(final Map<String, String> namespaceMappings,
											  final Map<String, String> reverseNamespaceMappings,
											  final Class<?>			resourceClass,
											  final Method				method,
											  final Object				object,
											  final Map<String, Object> nestedProperties,
											  final boolean				onlyNested)
			throws DatatypeConfigurationException,
				   IllegalAccessException,
				   IllegalArgumentException,
				   InvocationTargetException,
				   JSONException,
				   OslcCoreApplicationException
	{
		// Handle special float values.
		if ((object instanceof Float) &&
			writeSpecialNumberValues())
		{
			if (onlyNested)
			{
				return null;
			}

			final Float f = (Float) object;
			if (f.compareTo(Float.POSITIVE_INFINITY) == 0)
			{
				return POSITIVE_INF;
			}

			if (f.compareTo(Float.NEGATIVE_INFINITY) == 0)
			{
				return NEGATIVE_INF;
			}

			if (f.isNaN())
			{
				return NOT_A_NUMBER;
			}
		}

		// Handle special double values.
		if ((object instanceof Double) &&
			writeSpecialNumberValues())
		{
			if (onlyNested)
			{
				return null;
			}

			final Double d = (Double) object;
			if (d.compareTo(Double.POSITIVE_INFINITY) == 0)
			{
				return POSITIVE_INF;
			}

			if (d.compareTo(Double.NEGATIVE_INFINITY) == 0)
			{
				return NEGATIVE_INF;
			}

			if (d.isNaN())
			{
				return NOT_A_NUMBER;
			}
		}

		if ((object instanceof String)	||
			(object instanceof Boolean) ||
			(object instanceof Number))
		{
			if (onlyNested)
			{
				return null;
			}

			return object;
		}
		else if (object instanceof URI)
		{
			if (onlyNested)
			{
				return null;
			}

			return handleResourceReference(namespaceMappings,
										   reverseNamespaceMappings,
										   resourceClass,
										   method,
										   (URI) object);
		}
		else if (object instanceof Date)
		{
			if (onlyNested)
			{
				return null;
			}

			final GregorianCalendar calendar = new GregorianCalendar();
			calendar.setTime((Date) object);

			return DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar).toString();
		}
		else if (object instanceof IReifiedResource)
		{
			return handleReifiedResource(namespaceMappings,
										 reverseNamespaceMappings,
										 object.getClass(),
										 method,
										 (IReifiedResource<?>) object,
										 nestedProperties);
		}

		Map<Object, JSONObject> visitedObjects = new HashMap<Object,JSONObject>();
		return handleSingleResource(object,
									new JSONObject(),
									namespaceMappings,
									reverseNamespaceMappings,
									nestedProperties,
									visitedObjects);
	}

	private static Object handleReifiedResource(final Map<String, String> namespaceMappings,
												final Map<String, String> reverseNamespaceMappings,
												final Class<?>			  resourceClass,
												final Method			  method,
												final IReifiedResource<?> reifiedResource,
												final Map<String, Object> properties)
			throws OslcCoreInvalidPropertyTypeException,
				   OslcCoreRelativeURIException,
				   JSONException,
				   IllegalAccessException,
				   InvocationTargetException,
				   DatatypeConfigurationException,
				   OslcCoreApplicationException
	{
		final Object value = reifiedResource.getValue();
		if (value == null)
		{
			return null;
		}

		if (!(value instanceof URI))
		{
			// The OSLC JSON serialization doesn't support reification on anything except
			// resources by reference (typically links with labels). Throw an exception
			// if the value isn't a URI.
			// See http://open-services.net/bin/view/Main/OslcCoreSpecAppendixLinks
			throw new OslcCoreInvalidPropertyTypeException(resourceClass,
														   method,
														   method.getReturnType());
		}

		// Add the resource reference value.
		final JSONObject jsonObject = handleResourceReference(namespaceMappings,
															  reverseNamespaceMappings,
															  resourceClass,
															  method,
															  (URI) value);

		// Add any reified statements.
		Map<Object, JSONObject> visitedObjects = new HashMap<Object,JSONObject>();
		buildResourceAttributes(namespaceMappings,
								reverseNamespaceMappings,
								reifiedResource,
								resourceClass,
								jsonObject,
								properties,
								visitedObjects);

		return jsonObject;
	}

	protected static JSONObject handleResourceReference(final Map<String, String> namespaceMappings,
														final Map<String, String> reverseNamespaceMappings,
														final Class<?>			  resourceClass,
														final Method			  method,
														final URI				  uri)
			throws OslcCoreRelativeURIException,
				   JSONException
	{
		if (OSLC4JUtils.relativeURIsAreDisabled() && !uri.isAbsolute())
		{
			throw new OslcCoreRelativeURIException(resourceClass,
												   (method == null) ? "<none>" : method.getName(),
												   uri);
		}

		// Special nested JSONObject for URI
		final JSONObject jsonObject = new JSONObject();

		// Ensure we have an rdf prefix
		final String rdfPrefix = ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
													   OslcConstants.RDF_NAMESPACE,
													   namespaceMappings,
													   reverseNamespaceMappings);

		jsonObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
					   uri.toString());

		return jsonObject;
	}

	private static JSONObject handleSingleResource(final Object				 object,
												   final JSONObject			 jsonObject,
												   final Map<String, String> namespaceMappings,
												   final Map<String, String> reverseNamespaceMappings,
												   final Map<String, Object> properties,
												   final Map<Object,JSONObject> visitedObjects)
			throws DatatypeConfigurationException,
				   IllegalAccessException,
				   IllegalArgumentException,
				   InvocationTargetException,
				   JSONException,
				   OslcCoreApplicationException
	{
		final Class<? extends Object> objectClass = object.getClass();

		if (object instanceof URI) {

			// Ensure we have an rdf prefix
			final String rdfPrefix = ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
														   OslcConstants.RDF_NAMESPACE,
														   namespaceMappings,
														   reverseNamespaceMappings);

			jsonObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE,
					((URI) object).toASCIIString());

			visitedObjects.put(object, jsonObject);

		}
		else {

			// Collect the namespace prefix -> namespace mappings
			recursivelyCollectNamespaceMappings(namespaceMappings,
												reverseNamespaceMappings,
												objectClass);

			if (object instanceof IResource)
			{
				final URI aboutURI = ((IResource) object).getAbout();
				addAboutURI(jsonObject,
							namespaceMappings,
							reverseNamespaceMappings,
							objectClass,
							aboutURI);
			}

			buildResource(namespaceMappings,
						  reverseNamespaceMappings,
						  object,
						  objectClass,
						  jsonObject,
						  properties,
						  visitedObjects);
		}

		return jsonObject;
	}

	protected static void addAboutURI(final JSONObject jsonObject,
									  final Map<String, String> namespaceMappings,
									  final Map<String, String> reverseNamespaceMappings,
									  final Class<? extends Object> objectClass,
									  final URI aboutURI)
			throws OslcCoreRelativeURIException,
				   JSONException
	{
		if (aboutURI != null)
		{
			if (OSLC4JUtils.relativeURIsAreDisabled() && !aboutURI.isAbsolute())
			{
				throw new OslcCoreRelativeURIException(objectClass,
													   "getAbout",
													   aboutURI);
			}

			// Ensure we have an rdf prefix
			final String rdfPrefix = ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
														   OslcConstants.RDF_NAMESPACE,
														   namespaceMappings,
														   reverseNamespaceMappings);

			jsonObject.put(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_ABOUT,
						   aboutURI.toString());
		}
	}

	private static String ensureNamespacePrefix(final String			  prefix,
												final String			  namespace,
												final Map<String, String> namespaceMappings,
												final Map<String, String> reverseNamespaceMappings)
	{
		final String existingPrefix = reverseNamespaceMappings.get(namespace);

		if (existingPrefix != null)
		{
			return existingPrefix;
		}

		final String existingNamespace = namespaceMappings.get(prefix);

		if (existingNamespace == null)
		{
			namespaceMappings.put(prefix,
								  namespace);

			reverseNamespaceMappings.put(namespace,
										 prefix);

			return prefix;
		}

		// There is already a namespace for this prefix.  We need to generate a new unique prefix.
		int index = 1;

		while (true)
		{
			final String newPrefix = prefix +
									 index;

			if (!namespaceMappings.containsKey(newPrefix))
			{
				namespaceMappings.put(newPrefix,
									  namespace);

				reverseNamespaceMappings.put(namespace,
											 newPrefix);

				return newPrefix;
			}

			index++;
		}
	}

	private static void recursivelyCollectNamespaceMappings(final Map<String, String>	  namespaceMappings,
															final Map<String, String>	  reverseNamespaceMappings,
															final Class<? extends Object> objectClass)
	{
		final OslcSchema oslcSchemaAnnotation = objectClass.getPackage().getAnnotation(OslcSchema.class);
		if (oslcSchemaAnnotation != null)
		{
			final OslcNamespaceDefinition[] oslcNamespaceDefinitionAnnotations = oslcSchemaAnnotation.value();
			for (final OslcNamespaceDefinition oslcNamespaceDefinitionAnnotation : oslcNamespaceDefinitionAnnotations)
			{
				final String prefix		  = oslcNamespaceDefinitionAnnotation.prefix();
				final String namespaceURI = oslcNamespaceDefinitionAnnotation.namespaceURI();

				namespaceMappings.put(prefix,
									  namespaceURI);

				reverseNamespaceMappings.put(namespaceURI,
											 prefix);
			}
			//Adding custom prefixes obtained from an implementation, if there is an implementation.
			Class<? extends IOslcCustomNamespaceProvider> customNamespaceProvider = oslcSchemaAnnotation.customNamespaceProvider();
			if(!customNamespaceProvider.isInterface())
			{
				try {
					IOslcCustomNamespaceProvider customNamespaceProviderImpl = customNamespaceProvider.newInstance();
					Map<String, String> customNamespacePrefixes = customNamespaceProviderImpl.getCustomNamespacePrefixes();
					if(null != customNamespacePrefixes)
					{
						for(Map.Entry<String, String> namespaceEntry : customNamespacePrefixes.entrySet())
						{
							namespaceMappings.put(namespaceEntry.getKey(), namespaceEntry.getValue());
							reverseNamespaceMappings.put(namespaceEntry.getValue(), namespaceEntry.getKey());
						}
					}
				} catch (IllegalAccessException e) {
					throw new RuntimeException("The custom namespace provider implementation: "+
											   customNamespaceProvider.getClass().getName() +
											   ", must have a public no args construtor", e);
				} catch (InstantiationException e) {
					throw new RuntimeException("The custom namespace provider must not be a abstract, nor interface class and " +
											   "must have a public no args constructor", e);
				}
			}
		}

		final Class<?> superClass = objectClass.getSuperclass();
		if (superClass != null)
		{
			recursivelyCollectNamespaceMappings(namespaceMappings,
												reverseNamespaceMappings,
												superClass);
		}

		final Class<?>[] interfaces = objectClass.getInterfaces();
		if (interfaces != null)
		{
			for (final Class<?> interfac : interfaces)
			{
				recursivelyCollectNamespaceMappings(namespaceMappings,
													reverseNamespaceMappings,
													interfac);
			}
		}
	}

	/**
	 * Returns a list of rdf:types for a given json object. If the list was
	 * populated before, returns the given list. This list will only be
	 * populated if the property inferTypeFromShape is set to true.
	 *
	 * @param jsonObject
	 * @param rdfPrefix
	 * @param types
	 * @return List of rdf:types
	 */
	private static HashSet<String> getRdfTypesFromJsonObject(JSONObject jsonObject, String rdfPrefix, HashSet<String> types) {
		// The list of rdf:types will be populated only if the property
		// inferTypeFromShape is set and if the list was not populated before.
		// This is necessary because for an inline object, the retuned
		// rdf:type is not from the parent object, it is from the actual
		// resource.
		if (OSLC4JUtils.inferTypeFromShape() && types.isEmpty()) {
			final String typeProperty = rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TYPE;
			if (jsonObject.has(typeProperty)) {
				try {
					JSONArray array = jsonObject.getJSONArray(typeProperty);
					for (int i = 0; i < array.size(); ++i) {
						final JSONObject typeObj = array.getJSONObject(i);
						String resTypePropertyValue = typeObj.getString(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE);
						types.add(resTypePropertyValue);
					}
				}
				catch (JSONException e)
				{
					throw new IllegalArgumentException(e);
				}
			}
		}
		return types;
	}

	private static void fromJSON(final String							  rdfPrefix,
								 final Map<String, String>				  jsonNamespaceMappings,
								 final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods,
								 final JSONObject						  jsonObject,
								 final Class<?>							  beanClass,
								 final Object							  bean,
								 HashSet<String>						  rdfTypes)
			throws DatatypeConfigurationException,
				   IllegalAccessException,
				   IllegalArgumentException,
				   InstantiationException,
				   InvocationTargetException,
				   OslcCoreApplicationException,
				   URISyntaxException
	{
		Map<String, Method> setMethodMap = classPropertyDefinitionsToSetMethods.get(beanClass);
		if (setMethodMap == null)
		{
			setMethodMap = createPropertyDefinitionToSetMethods(beanClass);

			classPropertyDefinitionsToSetMethods.put(beanClass,
													 setMethodMap);
		}

		boolean isIReifiedResource = false;

		if (bean instanceof IResource)
		{
			final Object aboutURIObject = jsonObject.opt(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_ABOUT);

			if (aboutURIObject instanceof String)
			{
				final URI aboutURI = new URI(aboutURIObject.toString());

				if (OSLC4JUtils.relativeURIsAreDisabled() && !aboutURI.isAbsolute())
				{
					throw new OslcCoreRelativeURIException(beanClass,
														   "setAbout",
														   aboutURI);
				}

				((IResource) bean).setAbout(aboutURI);
			}
		}
		else if (bean instanceof IReifiedResource)
		{
			isIReifiedResource = true;

			@SuppressWarnings("unchecked")
			final IReifiedResource<Object> reifiedResource = (IReifiedResource<Object>) bean;
			String resourceReference;
			try
			{
				resourceReference = jsonObject.getString(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE);
			}
			catch (JSONException e)
			{
				throw new IllegalArgumentException(e);
			}

			try
			{
				reifiedResource.setValue(new URI(resourceReference));
			}
			catch (ClassCastException e)
			{
				throw new IllegalArgumentException(e);
			}
		}

		final IExtendedResource extendedResource;
		final Map<QName, Object> extendedProperties;
		if (bean instanceof IExtendedResource)
		{
			extendedResource = (IExtendedResource) bean;
			extendedProperties = new HashMap<QName, Object>();
			extendedResource.setExtendedProperties(extendedProperties);
		}
		else
		{
			extendedResource = null;
			extendedProperties = null;
		}

		// get the list of rdf types
		rdfTypes = getRdfTypesFromJsonObject(jsonObject, rdfPrefix, rdfTypes);

		@SuppressWarnings("unchecked")
		final Set<Map.Entry<String, Object>> entrySet = jsonObject.entrySet();

		for (final Map.Entry<String, Object> entry : entrySet)
		{
			final String prefixedName = entry.getKey();
			final Object jsonValue	  = entry.getValue();

			final String[] split = prefixedName.split(JSON_PROPERTY_DELIMITER);

			if (split.length != 2)
			{
				if (!JSON_PROPERTY_PREFIXES.equals(prefixedName))
				{
					logger.warning("Ignored JSON property '" +
								   prefixedName +
								   "'.");
				}
			}
			else
			{
				final String namespacePrefix = split[0];
				final String name			 = split[1];

				final String namespace = jsonNamespaceMappings.get(namespacePrefix);

				if (namespace == null)
				{
					throw new OslcCoreMissingNamespacePrefixException(namespacePrefix);
				}

				final String propertyDefinition = namespace +
												  name;

				final Method setMethod = setMethodMap.get(propertyDefinition);
				if (setMethod == null)
				{
					if (RDF_ABOUT_URI.equals(propertyDefinition) ||
						(isIReifiedResource && RDF_RESOURCE_URI.equals(propertyDefinition)))
					{
						// Ignore missing property definitions for rdf:about, rdf:types and
						// rdf:resource for IReifiedResources.
					}
					else if (RDF_TYPE_URI.equals(propertyDefinition))
					{
						if (extendedResource != null)
						{
							fillInRdfType(rdfPrefix, jsonObject, extendedResource);
						}
						// Otherwise ignore missing propertyDefinition for rdf:type.
					}
					else
					{
						if (extendedProperties == null)
						{
							logger.fine("Set method not found for object type:	" +
									beanClass.getName() +
									", propertyDefinition:	" +
									propertyDefinition);
						}
						else
						{
							final QName qName = new QName(namespace,
									  name,
									  namespacePrefix);

							final Object value = fromExtendedJSONValue(jsonValue,
																	   rdfPrefix,
																	   jsonNamespaceMappings,
																	   beanClass,
																	   qName,
																	   rdfTypes);
							extendedProperties.put(qName, value);
						}
					}
				}
				else
				{
					final Class<?> setMethodParameterClass = setMethod.getParameterTypes()[0];
					Class<?> setMethodComponentParameterClass = setMethodParameterClass;

					if (setMethodComponentParameterClass.isArray())
					{
						setMethodComponentParameterClass = setMethodComponentParameterClass.getComponentType();
					}
					else if (Collection.class.isAssignableFrom(setMethodComponentParameterClass))
					{
						final Type genericParameterType = setMethod.getGenericParameterTypes()[0];

						if (genericParameterType instanceof ParameterizedType)
						{
							final ParameterizedType parameterizedType = (ParameterizedType) genericParameterType;
							final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
							if (actualTypeArguments.length == 1)
							{
								final Type actualTypeArgument = actualTypeArguments[0];
								if (actualTypeArgument instanceof Class)
								{
									setMethodComponentParameterClass = (Class<?>) actualTypeArgument;
								}
							}
						}
					}

					final Object parameter = fromJSONValue(rdfPrefix,
														   jsonNamespaceMappings,
														   classPropertyDefinitionsToSetMethods,
														   beanClass,
														   setMethod,
														   setMethodParameterClass,
														   setMethodComponentParameterClass,
														   jsonValue,
														   rdfTypes);

					if (parameter != null)
					{
						setMethod.invoke(bean,
										 new Object[] {parameter});
					}
				}
			}
		}
	}

	/*
	 * Infer the appropriate bean value from the JSON value. We can't rely on
	 * the setter parameter type since this is an extended value that has no
	 * setter in the bean.
	 */
	private static Object fromExtendedJSONValue(final Object jsonValue,
												final String rdfPrefix,
												final Map<String, String> jsonNamespaceMappings,
												final Class<?> beanClass,
												final QName propertyQName,
												HashSet<String> rdfTypes)
			throws DatatypeConfigurationException,
				   URISyntaxException,
				   IllegalArgumentException,
				   IllegalAccessException,
				   InstantiationException,
				   InvocationTargetException,
				   OslcCoreApplicationException
	{
		if (jsonValue instanceof JSONArray)
		{
			final JSONArray jsonArray = (JSONArray) jsonValue;
			final ArrayList<Object> collection = new ArrayList<Object>();
			final Iterator<?> i = jsonArray.iterator();
			while (i.hasNext())
			{
				collection.add(fromExtendedJSONValue(i.next(), rdfPrefix, jsonNamespaceMappings, beanClass, propertyQName, rdfTypes));
			}

			return collection;
		}
		else if (jsonValue instanceof JSONObject)
		{
			final JSONObject o = (JSONObject) jsonValue;

			// Is it a resource reference?
			final Object resourceURIValue = o.opt(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE);
			if (resourceURIValue != null)
			{
				final URI uri = new URI((String) resourceURIValue);
				if (OSLC4JUtils.relativeURIsAreDisabled() && !uri.isAbsolute())
				{
					throw new OslcCoreRelativeURIException(beanClass,
														   "<none>",
														   uri);
				}

				return new URI((String) resourceURIValue);
			}

			// Handle an inline resource.
			final AbstractResource any = new AnyResource();
			fromJSON(rdfPrefix,
					 jsonNamespaceMappings,
					 new HashMap<Class<?>, Map<String, Method>>(),
					 o,
					 AnyResource.class,
					 any,
					 rdfTypes);

			return any;
		}
		else if (jsonValue instanceof String)
		{

			// fix for Bug 412789
			// try to infer the data type from resource shapes for Strings
			if (OSLC4JUtils.inferTypeFromShape()) {

				Object newObject = OSLC4JUtils.getValueBasedOnResourceShapeType(rdfTypes, propertyQName, jsonValue);

				// return the value only if the type was really inferred from
				// the resource shape, otherwise keep the same behavior
				if (null != newObject) {

					// return the new value only for ambiguous case
					if ((newObject instanceof String)
							|| (newObject instanceof XMLLiteral)
							|| (newObject instanceof Date)) {
						return newObject;
					}
				}
			}

			// Check if it's in the OSLC date format.
			try
			{
				return DatatypeFactory.newInstance()
						.newXMLGregorianCalendar((String) jsonValue)
						.toGregorianCalendar().getTime();
			}
			catch (IllegalArgumentException e)
			{
				// It's not a date. Treat it as a string.
				return jsonValue;
			}
		}
		else if (jsonValue instanceof Integer) {

			// fix for Bug 412789
			// There is no need to infer data type from resource shapes as
			// integer values do not have ambiguity cases
			return jsonValue;

		}
		else if (jsonValue instanceof Double) {

			// fix for Bug 412789
			// try to infer data type from resource shapes for Double
			if (OSLC4JUtils.inferTypeFromShape()) {
				Object newObject = OSLC4JUtils.getValueBasedOnResourceShapeType(rdfTypes, propertyQName, jsonValue);

				// return the value only if the type was really inferred from
				// the resource shape, otherwise keep the same behavior
				if (null != newObject) {

					// return the new value only for ambiguous case
					if ((newObject instanceof Double)
							|| (newObject instanceof Float)
							|| (newObject instanceof BigDecimal)) {
						return newObject;
					}
				}
			}

		}

		return jsonValue;
	}

	protected static void fillInRdfType(final String rdfPrefix,
										final JSONObject jsonObject,
										final IExtendedResource resource)
			throws URISyntaxException
	{
		final String typeProperty = rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_TYPE;
		if (jsonObject.has(typeProperty))
		{
			try
			{
				JSONArray array = jsonObject.getJSONArray(typeProperty);
				for (int i = 0; i < array.size(); ++i)
				{
					final JSONObject typeObj = array.getJSONObject(i);
					resource.addType(new URI(typeObj.getString(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE)));
				}
			}
			catch (JSONException e)
			{
				throw new IllegalArgumentException(e);
			}
		}
	}

	private static boolean isRdfListNode(final String				rdfPrefix,
										 final Class<?>				beanClass,
										 final Method				setMethod,
										 final Object				jsonValue)
	{
		if (!(jsonValue instanceof JSONObject))
		{
			return false;
		}

		final JSONObject jsonObject = (JSONObject)jsonValue;

		final boolean isListNode =
				jsonObject.has(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_FIRST)
				&& jsonObject.has(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_REST);
		if (isListNode)
		{
			return true;
		}

		final boolean isNilResource = RDF_NIL_URI.equals(
				jsonObject.optString(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE));
		if (!isNilResource)
		{
			return false;
		}

		final String setMethodName = setMethod.getName();
		if (setMethodName.startsWith(METHOD_NAME_START_SET)) {
			String getMethodName = METHOD_NAME_START_GET + setMethodName.substring(METHOD_NAME_START_GET_LENGTH);
			Method getMethod;
			try {
				getMethod = beanClass.getMethod(getMethodName, new Class[0]);
			} catch (NoSuchMethodException e) {

				String isMethodName = METHOD_NAME_START_IS + setMethodName.substring(METHOD_NAME_START_GET_LENGTH);
				try {
					getMethod = beanClass.getMethod(isMethodName, new Class[0]);
				} catch (NoSuchMethodException e1) {
					return false;
			   }
			}

			final OslcRdfCollectionType collectionType =
				InheritedMethodAnnotationHelper.getAnnotation(getMethod,
															  OslcRdfCollectionType.class);

			if (collectionType != null &&
					OslcConstants.RDF_NAMESPACE.equals(collectionType.namespaceURI()) &&
					"List".equals(collectionType.collectionType()))
			{
				return true;
			}
		}

		return false;
	}

	private static Object fromJSONValue(final String							 rdfPrefix,
										final Map<String, String>				 jsonNamespaceMappings,
										final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods,
										final Class<?>							 beanClass,
										final Method							 setMethod,
										final Class<?>							 setMethodParameterClass,
										final Class<?>							 setMethodComponentParameterClass,
										final Object							 jsonValue,
										HashSet<String>							 rdfTypes)
			throws DatatypeConfigurationException,
				   IllegalAccessException,
				   IllegalArgumentException,
				   InstantiationException,
				   InvocationTargetException,
				   OslcCoreApplicationException,
				   URISyntaxException
	{
		boolean isRdfContainerNode = isRdfListNode(rdfPrefix, beanClass, setMethod, jsonValue);
		JSONArray container = null;

		if (! isRdfContainerNode && jsonValue instanceof JSONObject) {

			JSONObject parent = (JSONObject)jsonValue;

			try
			{
				container = parent.optJSONArray(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_ALT,
												null);

				if (container == null)
				{
					container = parent.optJSONArray(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_BAG,
													null);
				}

				if (container == null)
				{
					container = parent.optJSONArray(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_SEQ,
													null);
				}
			}
			catch (JSONException e)
			{
				throw new IllegalArgumentException(e);
			}

			isRdfContainerNode = container != null;
		}

		if (!isRdfContainerNode && jsonValue instanceof JSONObject)
		{
			final JSONObject nestedJSONObject = (JSONObject) jsonValue;

			if (!IReifiedResource.class.isAssignableFrom(setMethodComponentParameterClass))
			{
				// If this is the special case for an rdf:resource?
				final Object uriObject = nestedJSONObject.opt(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE);

				if (uriObject instanceof String)
				{
					final URI uri = new URI(uriObject.toString());

					if (OSLC4JUtils.relativeURIsAreDisabled() && !uri.isAbsolute())
					{
						throw new OslcCoreRelativeURIException(beanClass,
								setMethod.getName(),
								uri);
					}

					return uri;
				}
			}

			final Object nestedBean = setMethodComponentParameterClass.newInstance();

			fromJSON(rdfPrefix,
					 jsonNamespaceMappings,
					 classPropertyDefinitionsToSetMethods,
					 nestedJSONObject,
					 setMethodComponentParameterClass,
					 nestedBean,
					 rdfTypes);

			return nestedBean;
		}
		else if (jsonValue instanceof JSONArray || isRdfContainerNode)
		{
			final Collection<Object> jsonArray;

			if (isRdfContainerNode && container == null)
			{
				jsonArray = new ArrayList<Object>();

				JSONObject listNode = (JSONObject) jsonValue;
				while (listNode != null
						&& !RDF_NIL_URI.equals(
							listNode.opt(rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_RESOURCE))) {

					Object o = listNode.opt(
							rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_FIRST);
					jsonArray.add(o);

					listNode = listNode.optJSONObject(
							rdfPrefix + JSON_PROPERTY_DELIMITER + JSON_PROPERTY_SUFFIX_REST);
				}
			}
			else if (isRdfContainerNode)
			{
				@SuppressWarnings("unchecked")
				final Collection<Object> array = container;

				jsonArray = array;
			}
			else
			{
				@SuppressWarnings("unchecked")
				final Collection<Object> array = (JSONArray)jsonValue;

				jsonArray = array;
			}

			final ArrayList<Object> tempList = new ArrayList<Object>();

			for (final Object jsonArrayEntryObject : jsonArray)
			{
				final Object parameterArrayObject = fromJSONValue(rdfPrefix,
																  jsonNamespaceMappings,
																  classPropertyDefinitionsToSetMethods,
																  beanClass,
																  setMethod,
																  setMethodComponentParameterClass,
																  setMethodComponentParameterClass,
																  jsonArrayEntryObject,
																  rdfTypes);

				tempList.add(parameterArrayObject);
			}

			if (setMethodParameterClass.isArray())
			{
				// To support primitive arrays, we have to use Array reflection to set individual elements.	 We cannot use Collection.toArray.
				// Array.set will unwrap objects to their corresponding primitives.
				final Object array = Array.newInstance(setMethodComponentParameterClass,
													   jsonArray.size());

				int index = 0;

				for (final Object parameterArrayObject : tempList)
				{
					Array.set(array,
							  index,
							  parameterArrayObject);

					index++;
				}

				return array;
			}

			// This has to be a Collection

			final Collection<Object> collection;

			// Handle the Collection, List, Deque, Queue interfaces.
			// Handle the AbstractCollection, AbstractList, AbstractSequentialList classes
			if ((Collection.class			  == setMethodParameterClass) ||
				(List.class					  == setMethodParameterClass) ||
				(Deque.class				  == setMethodParameterClass) ||
				(Queue.class				  == setMethodParameterClass) ||
				(AbstractCollection.class	  == setMethodParameterClass) ||
				(AbstractList.class			  == setMethodParameterClass) ||
				(AbstractSequentialList.class == setMethodParameterClass))
			{
				collection = new LinkedList<Object>();
			}
			// Handle the Set interface
			// Handle the AbstractSet class
			else if ((Set.class			 == setMethodParameterClass) ||
					 (AbstractSet.class	 == setMethodParameterClass))
			{
				collection = new HashSet<Object>();
			}
			// Handle the SortedSet and NavigableSet interfaces
			else if ((SortedSet.class	 == setMethodParameterClass) ||
					 (NavigableSet.class == setMethodParameterClass))
			{
				collection = new TreeSet<Object>();
			}
			// Not handled above. Let's try newInstance with possible failure.
			else
			{
				@SuppressWarnings("unchecked")
				final Collection<Object> tempCollection = ((Collection<Object>) setMethodParameterClass.newInstance());
				collection = tempCollection;
			}

			collection.addAll(tempList);

			return collection;
		}
		else if (jsonValue == null)
		{
			if (Boolean.class == setMethodComponentParameterClass || Boolean.TYPE == setMethodComponentParameterClass)
			{
				throw new IllegalArgumentException("Boolean cannot be null.");
			}

			if (Double.TYPE == setMethodComponentParameterClass)
			{
				if (readSpecialNumberValues())
				{
					logger.warning("Null double value treated as NaN.");
					return Double.NaN;
				}
				else
				{
					throw new IllegalArgumentException("Null double value not allowed. You can change this behavior by setting system property of "
							+ OSLC4J_READ_SPECIAL_NUMS + " to true.");
				}
			}

			if (Float.TYPE == setMethodComponentParameterClass)
			{
				if (readSpecialNumberValues())
				{
					logger.warning("Null float value treated as NaN.");
					return Float.NaN;
				}
				else
				{
					throw new IllegalArgumentException("Null float value not allowed. You can change this behavior by setting system property of "
							+ OSLC4J_READ_SPECIAL_NUMS + " to true.");
				}
			}

			if (Short.TYPE == setMethodComponentParameterClass ||
				Integer.TYPE == setMethodComponentParameterClass ||
				Long.TYPE == setMethodComponentParameterClass)
			{
				throw new IllegalArgumentException("Null values not allowed for type " + setMethodComponentParameterClass);
			}

			return null;
		}
		else
		{
			final String stringValue = jsonValue.toString();

			if (String.class == setMethodComponentParameterClass)
			{
				return stringValue;
			}
			else if ((Boolean.class == setMethodComponentParameterClass) || (Boolean.TYPE == setMethodComponentParameterClass))
			{
				// Cannot use Boolean.parseBoolean since it supports case-insensitive TRUE.
				if (Boolean.TRUE.toString().equals(stringValue))
				{
					return Boolean.TRUE;
				}
				else if (Boolean.FALSE.toString().equals(stringValue))
				{
					return Boolean.FALSE;
				}
				else
				{
					throw new IllegalArgumentException("'" + stringValue + "' has wrong format for Boolean.");
				}
			}
			else if ((Byte.class == setMethodComponentParameterClass) || (Byte.TYPE == setMethodComponentParameterClass))
			{
				return Byte.valueOf(stringValue);
			}
			else if ((Short.class == setMethodComponentParameterClass) || (Short.TYPE == setMethodComponentParameterClass))
			{
				return Short.valueOf(stringValue);
			}
			else if ((Integer.class == setMethodComponentParameterClass) || (Integer.TYPE == setMethodComponentParameterClass))
			{
				return Integer.valueOf(stringValue);
			}
			else if ((Long.class == setMethodComponentParameterClass) || (Long.TYPE == setMethodComponentParameterClass))
			{
				return Long.valueOf(stringValue);
			}
			else if (BigInteger.class == setMethodComponentParameterClass)
			{
				return new BigInteger(stringValue);
			}
			else if ((Float.class == setMethodComponentParameterClass) || (Float.TYPE == setMethodComponentParameterClass))
			{
				if (readSpecialNumberValues())
				{
					if (POSITIVE_INF.equals(stringValue) || "Infinity".equals(stringValue))
					{
						return Float.POSITIVE_INFINITY;
					}
					if (NEGATIVE_INF.equals(stringValue) || "-Infinity".equals(stringValue))
					{
						return Float.NEGATIVE_INFINITY;
					}
					if (NOT_A_NUMBER.equals(stringValue))
					{
						return Float.NaN;
					}
				}

				return Float.valueOf(stringValue);
			}
			else if ((Double.class == setMethodComponentParameterClass) || (Double.TYPE == setMethodComponentParameterClass))
			{
				if (readSpecialNumberValues())
				{
					if (POSITIVE_INF.equals(stringValue) || "Infinity".equals(stringValue))
					{
						return Double.POSITIVE_INFINITY;
					}
					if (NEGATIVE_INF.equals(stringValue) || "-Infinity".equals(stringValue))
					{
						return Double.NEGATIVE_INFINITY;
					}
					if (NOT_A_NUMBER.equals(stringValue))
					{
						return Double.NaN;
					}
				}

				return Double.valueOf(stringValue);
			}
			else if (Date.class == setMethodComponentParameterClass)
			{
				return DatatypeFactory.newInstance().newXMLGregorianCalendar(stringValue).toGregorianCalendar().getTime();
			}
		}

		return null;
	}

	private static boolean readSpecialNumberValues()
	{
		return "true".equals(System.getProperty(OSLC4J_READ_SPECIAL_NUMS, "true"));
	}

	private static boolean writeSpecialNumberValues()
	{
		return "true".equals(System.getProperty(OSLC4J_WRITE_SPECIAL_NUMS, "true"));
	}

	private static Map<String, Method> createPropertyDefinitionToSetMethods(final Class<?> beanClass)
			throws OslcCoreApplicationException
	{
		final Map<String, Method> result = new HashMap<String, Method>();
		final Method[] methods = beanClass.getMethods();
		for (final Method method : methods)
		{
			if (method.getParameterTypes().length == 0)
			{
				final String getMethodName = method.getName();
				if (((getMethodName.startsWith(METHOD_NAME_START_GET)) &&
					 (getMethodName.length() > METHOD_NAME_START_GET_LENGTH)) ||
					((getMethodName.startsWith(METHOD_NAME_START_IS)) &&
					 (getMethodName.length() > METHOD_NAME_START_IS_LENGTH)))
				{
					final OslcPropertyDefinition oslcPropertyDefinitionAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method,
																																  OslcPropertyDefinition.class);

					if (oslcPropertyDefinitionAnnotation != null)
					{
						// We need to find the set companion setMethod
						final String setMethodName;
						if (getMethodName.startsWith(METHOD_NAME_START_GET))
						{
							setMethodName = METHOD_NAME_START_SET +
											getMethodName.substring(METHOD_NAME_START_GET_LENGTH);
						}
						else
						{
							setMethodName = METHOD_NAME_START_SET +
											getMethodName.substring(METHOD_NAME_START_IS_LENGTH);
						}

						final Class<?> getMethodReturnType = method.getReturnType();
						try
						{
							final Method setMethod = beanClass.getMethod(setMethodName,
																		 getMethodReturnType);

							result.put(oslcPropertyDefinitionAnnotation.value(),
									   setMethod);
						}
						catch (final NoSuchMethodException exception)
						{
							throw new OslcCoreMissingSetMethodException(beanClass,
																		method,
																		exception);
						}
					}
				}
			}
		}

		return result;
	}

}
