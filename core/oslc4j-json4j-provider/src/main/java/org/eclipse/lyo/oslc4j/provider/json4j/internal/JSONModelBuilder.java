/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.NestedWildcardProperties;
import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.SingletonWildcardProperties;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.exception.MessageExtractor;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyTypeException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreRelativeURIException;
import org.eclipse.lyo.oslc4j.core.model.IExtendedResource;
import org.eclipse.lyo.oslc4j.core.model.IReifiedResource;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;
import org.eclipse.lyo.oslc4j.core.model.TypeFactory;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.eclipse.lyo.oslc4j.provider.json4j.internal.JavaResourceShape;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.LyoProviderUtils;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.NamespaceMappings;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.PropertyAccessor;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.RdfCollections;
import org.eclipse.lyo.oslc4j.provider.json4j.JsonHelper;

import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonValue;

@SuppressWarnings("java:S3776") // Complex legacy class
public class JSONModelBuilder extends AbstractBuilder {

    private static final Logger LOGGER = LoggerFactory.getLogger(JSONModelBuilder.class);

    public static JsonObject build(String descriptionAbout, String responseInfoAbout, ResponseInfo<?> responseInfo, Object[] objects,
            Map<String, Object> properties) throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException,
            InvocationTargetException, OslcCoreApplicationException {

        return new JSONModelBuilder().buildResource(descriptionAbout, responseInfoAbout, responseInfo, objects, properties);
    }

    private JSONModelBuilder() {
        super(NamespaceMappings.global());
    }

    private JsonObject buildResource(String descriptionAbout, String responseInfoAbout, ResponseInfo<?> responseInfo, Object[] objects,
            Map<String, Object> properties) throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException,
            InvocationTargetException, OslcCoreApplicationException {

        JSONObject resultJSONObject = new JSONObject();

        if (descriptionAbout != null) {
            resultJSONObject.put(getRdfKey(PROPERTY_ABOUT), descriptionAbout);

            JSONArray jsonArray = new JSONArray();
            for (Object object : objects) {
                HashMap<Object, JSONObject> visitedObjects = new HashMap<>();
                JSONObject jsonObject = buildSingleResource(object, new JSONObject(), properties, visitedObjects);
                jsonArray.add(jsonObject);
            }

            /* Support for Container rdf:type */
            if (LyoProviderUtils.isQueryResultListAsContainer()) {

                JSONObject containerTypeJSONObject = new JSONObject();
                containerTypeJSONObject.put(getRdfKey(PROPERTY_RESOURCE), OslcConstants.TYPE_CONTAINER);

                JSONArray containerTypesJSONArray = new JSONArray();
                containerTypesJSONArray.add(containerTypeJSONObject);
                resultJSONObject.put(getRdfKey(PROPERTY_TYPE), containerTypesJSONArray);

                Map<Object, JSONObject> visitedObjects = new HashMap<>();
                buildExtendedProperties(resultJSONObject, responseInfo.getContainer(), properties, visitedObjects);
            }

            resultJSONObject.put(getRdfsKey(PROPERTY_MEMBER), jsonArray);

            if (responseInfoAbout != null) {

                JSONObject responseInfoJSONObject = new JSONObject();
                responseInfoJSONObject.put(getRdfKey(PROPERTY_ABOUT), responseInfoAbout);

                if (responseInfo != null) {
                    responseInfoJSONObject.put(getOslcKey(PROPERTY_TOTAL_COUNT),
                            responseInfo.totalCount() == null ? Integer.valueOf(objects.length) : responseInfo.totalCount());

                    if (responseInfo.nextPage() != null) {
                        JSONObject nextPageJSONObject = new JSONObject();
                        nextPageJSONObject.put(getRdfKey(PROPERTY_RESOURCE), responseInfo.nextPage());
                        responseInfoJSONObject.put(getOslcKey(PROPERTY_NEXT_PAGE), nextPageJSONObject);
                    }

                    JSONObject responseInfoTypeJSONObject = new JSONObject();
                    responseInfoTypeJSONObject.put(getRdfKey(PROPERTY_RESOURCE), OslcConstants.TYPE_RESPONSE_INFO);

                    JSONArray responseInfoTypesJSONArray = new JSONArray();
                    responseInfoTypesJSONArray.add(responseInfoTypeJSONObject);

                    responseInfoJSONObject.put(getRdfKey(PROPERTY_TYPE), responseInfoTypesJSONArray);
                    resultJSONObject.put(getOslcKey(PROPERTY_RESPONSE_INFO), responseInfoJSONObject);

                    Map<Object, JSONObject> visitedObjects = new HashMap<>();
                    buildExtendedProperties(responseInfoJSONObject, responseInfo, properties, visitedObjects);
                }
            }
        }

        // unique object?
        else if (objects.length == 1) {
            HashMap<Object, JSONObject> visitedObjects = new HashMap<>();
            buildSingleResource(objects[0], resultJSONObject, properties, visitedObjects);
        }

        // Set the namespace prefixes
        Map<String, String> namespaces = getNamespaceMappings().getMappings();
        if (!namespaces.isEmpty()) {
            JSONObject prefixes = new JSONObject();
            namespaces.forEach(prefixes::put);
            resultJSONObject.put(PREFIXES, prefixes);
        }

        return resultJSONObject.build();
    }

    private void buildAccessor(Class<?> resourceClass, PropertyAccessor accessor, JSONObject jsonObject, Object value,
            Map<String, Object> nestedProperties, boolean onlyNested) throws DatatypeConfigurationException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException, OslcCoreApplicationException {

        boolean isRdfContainer;
        OslcRdfCollectionType collectionType = accessor.getCollectionType().orElse(null);
        if ((collectionType != null) && RdfCollections.isRdfCollection(collectionType)) {
            isRdfContainer = true;
        } else {
            isRdfContainer = false;
        }

        Object localResourceValue;

        Method method = accessor.getGetter();
        Class<?> returnType = method.getReturnType();

        if (returnType.isArray() || Collection.class.isAssignableFrom(returnType)) {
            JSONArray jsonArray = new JSONArray();

            if (returnType.isArray()) {
                // We cannot cast to Object[] in case this is an array of primitives. We will use Array reflection instead.
                // Strange case about primitive arrays: they cannot be cast to Object[], but retrieving their individual elements
                // does not return primitives, but the primitive object wrapping counterparts like Integer, Byte, Double, etc.
                int length = Array.getLength(value);
                for (int index = 0; index < length; index++) {
                    Object object = Array.get(value, index);
                    Object localResource = buildValue(resourceClass, method, object, nestedProperties, onlyNested);
                    if (localResource != null) {
                        jsonArray.add(localResource);
                    }
                }
            } else {
                @SuppressWarnings("unchecked")
                Collection<Object> collection = (Collection<Object>) value;
                for (Object object : collection) {
                    Object localResource = buildValue(resourceClass, method, object, nestedProperties, onlyNested);
                    if (localResource != null) {
                        jsonArray.add(localResource);
                    }
                }
            }

            if (jsonArray.isEmpty()) {
                localResourceValue = null;
            } else {
                if (isRdfContainer) {
                    localResourceValue = buildContainer(collectionType, jsonArray);
                } else {
                    localResourceValue = jsonArray;
                }
            }
        } else {
            localResourceValue = buildValue(resourceClass, method, value, nestedProperties, onlyNested);
        }

        if (localResourceValue != null) {
            jsonObject.put(generateKey(accessor.getQName()), localResourceValue);
        }
    }

    private Object buildContainer(OslcRdfCollectionType collectionType, JSONArray jsonArray) {
        if (RdfCollections.RDF_LIST.equals(collectionType.collectionType())) {
            JSONObject listObject = new JSONObject();

            listObject.put(getRdfKey(PROPERTY_RESOURCE), OslcConstants.RDF_NAMESPACE + PROPERTY_NIL);

            for (int i = jsonArray.size() - 1; i >= 0; i--) {
                Object o = jsonArray.get(i);

                JSONObject newListObject = new JSONObject();
                newListObject.put(getRdfKey(PROPERTY_FIRST), o);
                newListObject.put(getRdfKey(PROPERTY_REST), listObject);

                listObject = newListObject;
            }

            return listObject;
        }

        JSONObject container = new JSONObject();
        container.put(getRdfKey(collectionType.collectionType()), jsonArray);
        return container;
    }

    private void buildResource(Object object, Class<?> objectClass, JSONObject jsonObject, Map<String, Object> properties,
            Map<Object, JSONObject> visitedObjects) throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException,
            InvocationTargetException, OslcCoreApplicationException {

        visitedObjects.put(object, jsonObject);
        buildResourceProperties(object, objectClass, jsonObject, properties, visitedObjects);

        JSONArray rdfTypesJSONArray = new JSONArray();

        if (objectClass.getAnnotation(OslcResourceShape.class) != null) {
            String qualifiedName = TypeFactory.getQualifiedName(objectClass);
            if (qualifiedName != null) {
                addType(rdfTypesJSONArray, qualifiedName);
            }
        }

        if (object instanceof IExtendedResource) {
            IExtendedResource extendedResource = (IExtendedResource) object;
            buildExtendedTypes(extendedResource, properties, rdfTypesJSONArray);
        }

        jsonObject.put(getRdfKey(PROPERTY_TYPE), rdfTypesJSONArray);
    }

    private void addType(JSONArray rdfTypesJSONArray, String typeURI) {
        JSONObject rdfTypeJSONObject = new JSONObject();
        rdfTypeJSONObject.put(getRdfKey(PROPERTY_RESOURCE), typeURI);
        rdfTypesJSONArray.add(rdfTypeJSONObject);
    }

    private void buildResourceProperties(Object object, Class<?> objectClass, JSONObject jsonObject, Map<String, Object> properties,
            Map<Object, JSONObject> visitedObjects)
            throws IllegalAccessException, InvocationTargetException, DatatypeConfigurationException, OslcCoreApplicationException {

        if (properties == OSLC4JConstants.OSL4J_PROPERTY_SINGLETON) {
            return;
        }

        // loop on getters defined on the class
        for (PropertyAccessor accessor : JavaResourceShape.valueOf(objectClass).getAccessors()) {

            // obtain the value from the getter
            Object value = accessor.getGetter().invoke(object);
            if (value != null) {

                Map<String, Object> nestedProperties = null;
                boolean onlyNested = false;

                if (properties != null) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> map = (Map<String, Object>) properties.get(accessor.getPropertyDefinition());

                    if (map != null) {
                        nestedProperties = map;
                    } else if ((properties instanceof SingletonWildcardProperties) && !(properties instanceof NestedWildcardProperties)) {
                        nestedProperties = OSLC4JConstants.OSL4J_PROPERTY_SINGLETON;
                    } else if (properties instanceof NestedWildcardProperties) {
                        nestedProperties = ((NestedWildcardProperties) properties).commonNestedProperties();
                        onlyNested = !(properties instanceof SingletonWildcardProperties);
                    } else {
                        continue;
                    }
                }

                buildAccessor(objectClass, accessor, jsonObject, value, nestedProperties, onlyNested);
            }
        }

        if (object instanceof IExtendedResource) {
            IExtendedResource extendedResource = (IExtendedResource) object;
            buildExtendedProperties(jsonObject, extendedResource, properties, visitedObjects);
        }
    }

    private void buildExtendedProperties(JSONObject jsonObject, IExtendedResource extendedResource, Map<String, Object> properties,
            Map<Object, JSONObject> visitedObjects)
            throws DatatypeConfigurationException, IllegalAccessException, InvocationTargetException, OslcCoreApplicationException {

        String rdfTypeKey = getRdfKey(PROPERTY_TYPE);
        JSONArray typesJSONArray;
        if (jsonObject.containsKey(rdfTypeKey)) {
            typesJSONArray = (JSONArray) jsonObject.get(rdfTypeKey);
        } else {
            typesJSONArray = new JSONArray();
        }

        buildExtendedTypes(extendedResource, properties, typesJSONArray);

        if (typesJSONArray.size() > 0) {
            jsonObject.put(getRdfKey(PROPERTY_TYPE), typesJSONArray);
        }

        if (extendedResource.getExtendedProperties() != null) {
            for (Map.Entry<QName, Object> extendedProperty : extendedResource.getExtendedProperties().entrySet()) {
                String namespace = extendedProperty.getKey().getNamespaceURI();
                String localName = extendedProperty.getKey().getLocalPart();
                Map<String, Object> nestedProperties = null;
                boolean onlyNested = false;

                if (properties != null) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> map = (Map<String, Object>) properties.get(namespace + localName);

                    if (map != null) {
                        nestedProperties = map;
                    } else if ((properties instanceof SingletonWildcardProperties) && !(properties instanceof NestedWildcardProperties)) {
                        nestedProperties = OSLC4JConstants.OSL4J_PROPERTY_SINGLETON;
                    } else if (properties instanceof NestedWildcardProperties) {
                        nestedProperties = ((NestedWildcardProperties) properties).commonNestedProperties();
                        onlyNested = !(properties instanceof SingletonWildcardProperties);
                    } else {
                        continue;
                    }
                }

                Object value = buildExtendedValue(extendedProperty.getValue(), nestedProperties, onlyNested, visitedObjects);

                if ((value == null) && !onlyNested) {
                    LOGGER.warn("Could not add extended property {} for resource {}", extendedProperty.getKey(), extendedResource.getAbout());
                } else {
                    // avoid serializing empty arrays
                    if (!((value instanceof JSONArray) && (((JSONArray) value).isEmpty()))) {
                        jsonObject.put(generateKey(extendedProperty.getKey()), value);
                    }
                }
            }
        }
    }

    private boolean isPropertyAccepted(Map<String, Object> properties, String propertyName) {
        return (properties == null) //
                || (properties.get(propertyName) != null) //
                || (properties instanceof NestedWildcardProperties) //
                || (properties instanceof SingletonWildcardProperties);
    }

    /*
     * Add properties for RDF types defined on the IExtendedResource
     */
    private void buildExtendedTypes(IExtendedResource extendedResource, Map<String, Object> properties, JSONArray typesJSONArray) {
        if (!isPropertyAccepted(properties, OslcConstants.RDF_NAMESPACE + PROPERTY_TYPE)) {
            return;
        }

        if ((extendedResource.getTypes() != null) && !extendedResource.getTypes().isEmpty()) {

            // make sure we don't add a type already part of the array
            Set<String> knownTypes = new HashSet<>();
            for (Object jsonValue : typesJSONArray) {
                String typeName = ((JSONObject) jsonValue).getString(getRdfKey(PROPERTY_RESOURCE));
                knownTypes.add(typeName);
            }

            for (URI type : extendedResource.getTypes()) {
                String typeName = type.toString();
                if (knownTypes.add(typeName)) {
                    addType(typesJSONArray, typeName);
                }
            }
        }
    }

    private Object buildExtendedValue(Object object, Map<String, Object> nestedProperties, boolean onlyNested, Map<Object, JSONObject> visitedObjects)
            throws DatatypeConfigurationException, IllegalArgumentException, IllegalAccessException, InvocationTargetException,
            OslcCoreApplicationException {

        if (object instanceof Collection) {
            JSONArray jsonArray = new JSONArray();
            @SuppressWarnings("unchecked")
            Collection<Object> c = (Collection<Object>) object;
            for (Object next : c) {
                Object nextJson = buildExtendedValue(next, nestedProperties, onlyNested, visitedObjects);
                if (nextJson != null) {
                    jsonArray.add(nextJson);
                }
            }
            if (RdfCollections.isSequence(c) && !c.isEmpty()) {
                JSONObject sequence = new JSONObject();
                sequence.put(getRdfKey(RdfCollections.RDF_SEQ), jsonArray);
                return sequence;
            } else {
                return jsonArray;
            }
        }

        else if ((object instanceof String) || (object instanceof Boolean) || (object instanceof Number)) {
            if (onlyNested) {
                return null;
            }
            return object;
        }

        else if (object instanceof XMLLiteral) {
            if (onlyNested) {
                return null;
            }

            // XMLLiterals are treated as strings in the OSLC 2.0 JSON format.
            return ((XMLLiteral) object).getValue();
        }

        else if (object instanceof Date) {
            if (onlyNested) {
                return null;
            }

            GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTime((Date) object);
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar).toString();
        }

        else if (object instanceof URI) {
            if (onlyNested) {
                return null;
            }
            return buildResourceReference(object.getClass(), null, (URI) object);
        }

        else if ((object instanceof IResource) && !visitedObjects.containsKey(object)) {
            return buildSingleResource(object, new JSONObject(), nestedProperties, visitedObjects);
        }

        else if (object instanceof IReifiedResource) {
            return buildReifiedResource(object.getClass(), null, (IReifiedResource<?>) object, nestedProperties);
        }

        else if (visitedObjects.containsKey(object)) {
            JSONObject returnObject = visitedObjects.get(object);
            if (!returnObject.isEmpty()) {
                return returnObject;
            }
        }

        return null;
    }

    private Object buildValue(Class<?> resourceClass, Method method, Object object, Map<String, Object> nestedProperties, boolean onlyNested)
            throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            OslcCoreApplicationException {

        // Handle special float values.
        if ((object instanceof Float) && writeSpecialNumberValues()) {
            if (onlyNested) {
                return null;
            }

            Float f = (Float) object;
            if (f.compareTo(Float.POSITIVE_INFINITY) == 0) {
                return POSITIVE_INF;
            }

            if (f.compareTo(Float.NEGATIVE_INFINITY) == 0) {
                return NEGATIVE_INF;
            }

            if (f.isNaN()) {
                return NOT_A_NUMBER;
            }
        }

        // Handle special double values.
        if ((object instanceof Double) && writeSpecialNumberValues()) {
            if (onlyNested) {
                return null;
            }

            Double d = (Double) object;
            if (d.compareTo(Double.POSITIVE_INFINITY) == 0) {
                return POSITIVE_INF;
            }

            if (d.compareTo(Double.NEGATIVE_INFINITY) == 0) {
                return NEGATIVE_INF;
            }

            if (d.isNaN()) {
                return NOT_A_NUMBER;
            }
        }

        if ((object instanceof String) || (object instanceof Boolean) || (object instanceof Number)) {
            if (onlyNested) {
                return null;
            }
            return object;
        }

        else if (object instanceof URI) {
            if (onlyNested) {
                return null;
            }
            return buildResourceReference(resourceClass, method, (URI) object);
        }

        else if (object instanceof Date) {
            if (onlyNested) {
                return null;
            }
            GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTime((Date) object);
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar).toString();
        }

        else if (object instanceof IReifiedResource) {
            return buildReifiedResource(object.getClass(), method, (IReifiedResource<?>) object, nestedProperties);
        }

        Map<Object, JSONObject> visitedObjects = new HashMap<>();
        return buildSingleResource(object, new JSONObject(), nestedProperties, visitedObjects);
    }

    private Object buildReifiedResource(Class<?> resourceClass, Method method, IReifiedResource<?> reifiedResource, Map<String, Object> properties)
            throws IllegalAccessException, InvocationTargetException, DatatypeConfigurationException, OslcCoreApplicationException {

        Object value = reifiedResource.getValue();
        if (value == null) {
            return null;
        }

        if (!(value instanceof URI)) {
            // The OSLC JSON serialization doesn't support reification on anything except
            // resources by reference (typically links with labels). Throw an exception
            // if the value isn't a URI.
            // See http://open-services.net/bin/view/Main/OslcCoreSpecAppendixLinks
            if (method == null) {
                // OslcCoreInvalidPropertyTypeException requires a method, which we don't have here.
                // Let's log the exception message
                Object[] messageArgs = new Object[] { resourceClass.getName(), "<none>", "<none>" }; // NOSONAR
                LOGGER.warn(MessageExtractor.getMessage("InvalidPropertyTypeException", messageArgs));
                return null;
            } else {
                throw new OslcCoreInvalidPropertyTypeException(resourceClass, method, method.getReturnType());
            }
        }

        // Add the resource reference value.
        JSONObject jsonObject = buildResourceReference(resourceClass, method, (URI) value);

        // Add any reified statements.
        Map<Object, JSONObject> visitedObjects = new HashMap<>();
        buildResourceProperties(reifiedResource, resourceClass, jsonObject, properties, visitedObjects);

        return jsonObject;
    }

    private JSONObject buildResourceReference(Class<?> resourceClass, Method method, URI uri) throws OslcCoreRelativeURIException {
        if (LyoProviderUtils.relativeURIsAreDisabled() && !uri.isAbsolute()) {
            throw new OslcCoreRelativeURIException(resourceClass, (method == null) ? "<none>" : method.getName(), uri);
        }

        JSONObject jsonObject = new JSONObject();
        jsonObject.put(getRdfKey(PROPERTY_RESOURCE), uri.toString());
        return jsonObject;
    }

    private JSONObject buildSingleResource(Object object, JSONObject jsonObject, Map<String, Object> properties,
            Map<Object, JSONObject> visitedObjects) throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException,
            InvocationTargetException, OslcCoreApplicationException {

        if (object instanceof URI) {
            jsonObject.put(getRdfKey(PROPERTY_RESOURCE), ((URI) object).toASCIIString());
            visitedObjects.put(object, jsonObject);
        } else {
            // Collect the namespace prefix -> namespace mappings
            Class<? extends Object> objectClass = object.getClass();
            getNamespaceMappings().addMappings(objectClass);

            if (object instanceof IResource) {
                URI aboutURI = ((IResource) object).getAbout();
                addAboutURI(jsonObject, objectClass, aboutURI);
            }

            buildResource(object, objectClass, jsonObject, properties, visitedObjects);
        }

        return jsonObject;
    }

    private void addAboutURI(JSONObject jsonObject, Class<? extends Object> objectClass, URI aboutURI) throws OslcCoreRelativeURIException {
        if (aboutURI != null) {
            if (LyoProviderUtils.relativeURIsAreDisabled() && !aboutURI.isAbsolute()) {
                throw new OslcCoreRelativeURIException(objectClass, "getAbout", aboutURI);
            }

            jsonObject.put(getRdfKey(PROPERTY_ABOUT), aboutURI.toString());
        }
    }

    private static boolean writeSpecialNumberValues() {
        return "true".equals(System.getProperty(JsonHelper.OSLC4J_WRITE_SPECIAL_NUMS, "true"));
    }

    private String getRdfKey(String localPart) {
        return generateKey(OslcConstants.RDF_NAMESPACE, localPart, OslcConstants.RDF_NAMESPACE_PREFIX);
    }

    private String getRdfsKey(String localPart) {
        return generateKey(OslcConstants.RDFS_NAMESPACE, localPart, OslcConstants.RDFS_NAMESPACE_PREFIX);
    }

    private String getOslcKey(String localPart) {
        return generateKey(OslcConstants.OSLC_CORE_NAMESPACE, localPart, OslcConstants.OSLC_CORE_NAMESPACE_PREFIX);
    }
}