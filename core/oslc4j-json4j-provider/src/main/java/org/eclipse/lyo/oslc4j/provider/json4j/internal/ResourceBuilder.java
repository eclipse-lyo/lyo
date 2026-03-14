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
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingNamespaceDeclarationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingNamespacePrefixException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMisusedOccursException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreRelativeURIException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.AnyResource;
import org.eclipse.lyo.oslc4j.core.model.IExtendedResource;
import org.eclipse.lyo.oslc4j.core.model.IReifiedResource;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.CollectionSetterInvoker;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.JavaResourceShape;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.LyoProviderUtils;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.NamespaceMappings;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.PropertyAccessor;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.RdfCollections;
import org.eclipse.lyo.oslc4j.provider.json4j.JsonHelper;

import jakarta.json.JsonArray;
import jakarta.json.JsonNumber;
import jakarta.json.JsonObject;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

@SuppressWarnings("java:S3776") // Complex legacy class
public final class ResourceBuilder extends AbstractBuilder {

    private static final String RDF_ABOUT_URI = OslcConstants.RDF_NAMESPACE + PROPERTY_ABOUT;
    private static final String RDF_TYPE_URI = OslcConstants.RDF_NAMESPACE + PROPERTY_TYPE;
    private static final String RDF_NIL_URI = OslcConstants.RDF_NAMESPACE + PROPERTY_NIL;
    private static final String RDF_RESOURCE_URI = OslcConstants.RDF_NAMESPACE + PROPERTY_RESOURCE;

    private static final Pattern NUMBER_PATTERN = Pattern.compile("\\d+");

    private static final Logger LOGGER = LoggerFactory.getLogger(ResourceBuilder.class);

    public static Object[] build(JsonObject jakartaObject, Class<?> beanClass) throws DatatypeConfigurationException, IllegalAccessException,
            IllegalArgumentException, InstantiationException, InvocationTargetException, OslcCoreApplicationException, URISyntaxException {

        try {
            return new ResourceBuilder().buildResources(jakartaObject, beanClass);
        } catch (NoSuchMethodException e) {
            throw new InvocationTargetException(e);
        }
    }

    private ResourceBuilder() {
        super(NamespaceMappings.empty());
    }

    private Object buildResource(JSONObject resourceJSONObject, Class<?> beanClass)
            throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException, InstantiationException,
            InvocationTargetException, OslcCoreApplicationException, URISyntaxException, NoSuchMethodException {

        Object bean = beanClass.getDeclaredConstructor().newInstance();
        Set<String> rdfTypes = new HashSet<>();
        buildResource(resourceJSONObject, beanClass, bean, rdfTypes);
        return bean;
    }

    private Object[] buildResources(JsonObject jakartaObject, Class<?> beanClass)
            throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException, InstantiationException,
            InvocationTargetException, OslcCoreApplicationException, URISyntaxException, NoSuchMethodException {

        List<Object> beans = new ArrayList<>();

        JSONObject jsonObject = new JSONObject(jakartaObject);

        // First read the prefixes and set up maps so we can create full property definition values later
        JsonValue prefixes = jakartaObject.get(PREFIXES);
        if (prefixes instanceof JsonObject) {
            JSONObject prefixesJSONObject = new JSONObject((JsonObject) prefixes);
            addMappings(prefixesJSONObject);
        }

        JsonArray jsonArray = null;

        // Look for rdfs:member
        if (getNamespaceMappings().containsPrefix(OslcConstants.RDFS_NAMESPACE_PREFIX)) {
            JsonValue members = jakartaObject.get(getRdfsKey(PROPERTY_MEMBER));
            if (members instanceof JsonArray) {

                // If the Java class defines an accessor for rdfs:member property,
                // the caller expects to get an instance of the shape with some members assigned (PRDOSLC-1376).
                // Otherwise the caller expects a collection of those members
                Optional<PropertyAccessor> membersAccessor = JavaResourceShape.valueOf(beanClass)
                        .getAccessor(OslcConstants.RDFS_NAMESPACE + PROPERTY_MEMBER);
                if (membersAccessor.isEmpty()) {
                    jsonArray = (JsonArray) members;
                }
            }
        }

        // Look for oslc:results. Seen in ChangeManagement.
        if ((jsonArray == null) && getNamespaceMappings().containsPrefix(OslcConstants.OSLC_CORE_NAMESPACE_PREFIX)) {
            JsonValue results = jakartaObject.get(getOslcKey(PROPERTY_RESULTS));
            if (results instanceof JsonArray) {
                jsonArray = (JsonArray) results;
            }
        }

        // array?
        if (jsonArray != null) {
            for (JsonValue arrayValue : jsonArray) {
                if (arrayValue instanceof JsonObject) {
                    JSONObject arrayObject = new JSONObject((JsonObject) arrayValue);
                    if (URI.class.equals(beanClass)) {
                        String uri = arrayObject.optString(getRdfKey(PROPERTY_RESOURCE));
                        beans.add(URI.create(uri));
                    } else {
                        Object resource = buildResource(arrayObject, beanClass);
                        beans.add(resource);
                    }
                }
            }
        }

        // resource
        else {
            Object resource = buildResource(jsonObject, beanClass);
            beans.add(resource);
        }

        return beans.toArray((Object[]) Array.newInstance(beanClass, beans.size()));
    }

    /**
     * Returns a list of rdf:types for a given json object. If the list was
     * populated before, returns the given list. This list will only be
     * populated if the property inferTypeFromShape is set to true.
     *
     * @param jsonObject
     * @param types
     * @return List of rdf:types
     * @throws OslcCoreMissingNamespaceDeclarationException
     */
    private Set<String> getRdfTypesFromJsonObject(JSONObject jsonObject, Set<String> types) throws OslcCoreMissingNamespaceDeclarationException {
        // The list of rdf:types will be populated only if the property
        // inferTypeFromShape is set and if the list was not populated before.
        // This is necessary because for an inline object, the retuned
        // rdf:type is not from the parent object, it is from the actual
        // resource.
        if (OSLC4JUtils.inferTypeFromShape() && types.isEmpty()) {
            String typeProperty = getRdfKey(PROPERTY_TYPE);
            if (jsonObject.has(typeProperty)) {
                JSONArray array = jsonObject.getJSONArray(typeProperty);
                for (int i = 0; i < array.size(); ++i) {
                    JSONObject typeObj = array.getJSONObject(i);
                    String resTypePropertyValue = typeObj.getString(getRdfKey(PROPERTY_RESOURCE));
                    types.add(resTypePropertyValue);
                }
            }
        }
        return types;
    }

    private void buildResource(JSONObject jsonObject, Class<?> beanClass, Object bean, Set<String> rdfTypes)
            throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException, InstantiationException,
            InvocationTargetException, OslcCoreApplicationException, URISyntaxException, NoSuchMethodException {

        boolean isIReifiedResource = false;

        // rdf:about
        if (bean instanceof IResource) {
            Object aboutURIObject = jsonObject.opt(getRdfKey(PROPERTY_ABOUT));

            if (aboutURIObject instanceof String) {
                URI aboutURI = new URI((String) aboutURIObject);
                if (LyoProviderUtils.relativeURIsAreDisabled() && !aboutURI.isAbsolute()) {
                    throw new OslcCoreRelativeURIException(beanClass, "setAbout", aboutURI);
                }
                ((IResource) bean).setAbout(aboutURI);
            }
        }

        // reified?
        else if (bean instanceof IReifiedResource) {
            isIReifiedResource = true;

            @SuppressWarnings("unchecked")
            IReifiedResource<Object> reifiedResource = (IReifiedResource<Object>) bean;
            String resourceReference = jsonObject.getString(getRdfKey(PROPERTY_RESOURCE));

            try {
                reifiedResource.setValue(new URI(resourceReference));
            } catch (ClassCastException e) {
                throw new IllegalArgumentException(e);
            }
        }

        IExtendedResource extendedResource;
        Map<QName, Object> extendedProperties;
        if (bean instanceof IExtendedResource) {
            extendedResource = (IExtendedResource) bean;
            extendedProperties = new HashMap<>();
            extendedResource.setExtendedProperties(extendedProperties);
        } else {
            extendedResource = null;
            extendedProperties = null;
        }

        // get the list of rdf types
        rdfTypes = getRdfTypesFromJsonObject(jsonObject, rdfTypes);

        for (Entry<String, JsonValue> entry : jsonObject.entrySet()) {
            String prefixedName = entry.getKey();
            JsonValue jsonValue = entry.getValue();

            // detect the property definition used
            String[] split = prefixedName.split(JSON_PROPERTY_DELIMITER);
            if (split.length != 2) {
                if (!PREFIXES.equals(prefixedName)) {
                    LOGGER.warn("Ignored JSON property '{}'.", prefixedName);
                }
            } else {
                QName qName = getExistingQName(split[0], split[1]);
                String propertyDefinition = qName.getNamespaceURI() + qName.getLocalPart();

                // no setter -> use an extended property
                Optional<PropertyAccessor> accessor = JavaResourceShape.valueOf(beanClass).getAccessor(propertyDefinition);
                if (!accessor.isPresent()) {
                    if (RDF_ABOUT_URI.equals(propertyDefinition) || (isIReifiedResource && RDF_RESOURCE_URI.equals(propertyDefinition))) {
                        // Ignore missing property definitions for rdf:about, rdf:types and
                        // rdf:resource for IReifiedResources.
                    } else if (RDF_TYPE_URI.equals(propertyDefinition)) {
                        if (extendedResource != null) {
                            fillInRdfType(jsonObject, extendedResource);
                        }
                        // Otherwise ignore missing propertyDefinition for rdf:type.
                    } else {
                        if (extendedProperties == null) {
                            LOGGER.debug("Set method not found for object type: {}, propertyDefinition: {}", beanClass.getName(), propertyDefinition);
                        } else {
                            Object value = buildExtendedValue(jsonValue, beanClass, qName, rdfTypes);
                            if (value != null) {
                                extendedProperties.put(qName, value);
                            }
                        }
                    }
                }

                // use the setter
                else {
                    buildAccessor(beanClass, bean, rdfTypes, jsonValue, accessor.get());
                }
            }
        }
    }

    private void buildAccessor(Class<?> beanClass, Object bean, Set<String> rdfTypes, JsonValue jsonValue, PropertyAccessor accessor)
            throws DatatypeConfigurationException, IllegalAccessException, InstantiationException, InvocationTargetException,
            OslcCoreApplicationException, URISyntaxException, NoSuchMethodException {

        Method setMethod = accessor.getSetter();
        Class<?> setMethodParameterClass = setMethod.getParameterTypes()[0];
        Class<?> setMethodComponentParameterClass = setMethodParameterClass;

        boolean multiple = false;
        if (setMethodComponentParameterClass.isArray()) {
            multiple = true;
            setMethodComponentParameterClass = setMethodComponentParameterClass.getComponentType();
        } else if (Collection.class.isAssignableFrom(setMethodComponentParameterClass)) {
            multiple = true;
            Type genericParameterType = setMethod.getGenericParameterTypes()[0];

            if (genericParameterType instanceof ParameterizedType) {
                ParameterizedType parameterizedType = (ParameterizedType) genericParameterType;
                Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
                if (actualTypeArguments.length == 1) {
                    Type actualTypeArgument = actualTypeArguments[0];
                    if (actualTypeArgument instanceof Class) {
                        setMethodComponentParameterClass = (Class<?>) actualTypeArgument;
                    }
                }
            }
        }

        // build the value
        Object parameter = buildAccessorValue(beanClass, accessor, setMethodParameterClass, setMethodComponentParameterClass, jsonValue, rdfTypes);
        if (parameter != null) {

            // If the method expects an array or collection and a single value was found,
            // use CollectionSetterInvoker to wrap the value and call the method.
            if (multiple && !((parameter instanceof Collection<?>) || parameter.getClass().isArray())) {
                CollectionSetterInvoker invoker = new CollectionSetterInvoker(bean);
                invoker.add(accessor, parameter);
                invoker.invokeAll();
            }

            // call the method with the value
            else {
                setMethod.invoke(bean, parameter);
            }
        }
    }

    /*
     * Infer the appropriate bean value from the JSON value. We can't rely on
     * the setter parameter type since this is an extended value that has no
     * setter in the bean.
     */
    private Object buildExtendedValue(JsonValue jsonValue, Class<?> beanClass, QName propertyQName, Set<String> rdfTypes)
            throws DatatypeConfigurationException, URISyntaxException, IllegalArgumentException, IllegalAccessException, InstantiationException,
            InvocationTargetException, OslcCoreApplicationException, NoSuchMethodException {

        // Json array?
        if (jsonValue instanceof JsonArray) {
            JsonArray array = (JsonArray) jsonValue;
            return buildExtendedArray(array, beanClass, propertyQName, rdfTypes);
        }

        // Json object?
        else if (jsonValue instanceof JsonObject) {
            JSONObject o = new JSONObject((JsonObject) jsonValue);
            return buildExtendedResource(o, beanClass, propertyQName, rdfTypes);
        }

        // string?
        else if (jsonValue instanceof JsonString) {
            String jsonStringValue = ((JsonString) jsonValue).getString();
            return buildExtendedString(jsonStringValue, propertyQName, rdfTypes);
        }

        // number?
        else if (jsonValue instanceof JsonNumber) {
            return buildExtendedNumber(((JsonNumber) jsonValue).numberValue(), propertyQName, rdfTypes);
        }

        // boolean?
        else if (JsonValue.TRUE.equals(jsonValue)) {
            return true;
        } else if (JsonValue.FALSE.equals(jsonValue)) {
            return false;
        }

        // null?
        else if ((jsonValue == null) || (JsonValue.NULL.equals(jsonValue))) {
            return null;
        }

        // what's that?!
        else {
            throw new IllegalStateException("Unexpected type of JsonValue: " + jsonValue.getClass().getName());
        }
    }

    @SuppressWarnings("java:S1130") // InvocationTargetException can be raised by early Lyo versions
    private Object buildExtendedNumber(Number value, QName propertyQName, Set<String> rdfTypes)
            throws DatatypeConfigurationException, InstantiationException, InvocationTargetException {

        if (value instanceof Integer || value instanceof Long || value instanceof Short || value instanceof Byte || value instanceof BigInteger) {
            // fix for Bug 412789
            // There is no need to infer data type from resource shapes as integer values do not have ambiguity cases
            return value;
        } else {
            // fix for Bug 412789
            // try to infer data type from resource shapes for Double
            if (OSLC4JUtils.inferTypeFromShape()) {
                Object newObject = OSLC4JUtils.getValueBasedOnResourceShapeType(new HashSet<>(rdfTypes), propertyQName, value.doubleValue());

                // return the value only if the type was really inferred from
                // the resource shape, otherwise keep the same behavior
                if (null != newObject) {

                    // return the new value only for ambiguous case
                    if ((newObject instanceof Double) || (newObject instanceof Float) || (newObject instanceof BigDecimal)) { // NOSONAR
                        return newObject;
                    }
                }
            }

            return value.doubleValue();
        }
    }

    @SuppressWarnings("java:S1130") // InvocationTargetException can be raised by early Lyo versions
    private Object buildExtendedString(String value, QName propertyQName, Set<String> rdfTypes)
            throws DatatypeConfigurationException, InstantiationException, InvocationTargetException {

        // fix for Bug 412789
        // try to infer the data type from resource shapes for Strings
        if (OSLC4JUtils.inferTypeFromShape()) {
            Object newObject = OSLC4JUtils.getValueBasedOnResourceShapeType(new HashSet<>(rdfTypes), propertyQName, value);

            // return the value only if the type was really inferred from
            // the resource shape, otherwise keep the same behavior
            if (null != newObject) {

                // return the new value only for ambiguous case
                if ((newObject instanceof String) || (newObject instanceof XMLLiteral) || (newObject instanceof Date)) { // NOSONAR
                    return newObject;
                }
            }
        }

        // If it's a number, don't try converting to a date (PRDOSLC-723)
        if (NUMBER_PATTERN.matcher(value).matches()) {
            return value;
        }

        // Check if it's in the OSLC date format.
        try {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(value).toGregorianCalendar().getTime();
        } catch (IllegalArgumentException e) {
            // It's not a date. Treat it as a string.
            return value;
        }
    }

    private Object buildExtendedResource(JSONObject o, Class<?> beanClass, QName propertyQName, Set<String> rdfTypes)
            throws URISyntaxException, DatatypeConfigurationException, IllegalAccessException, InstantiationException, InvocationTargetException,
            OslcCoreApplicationException, NoSuchMethodException {

        // resource reference?
        String resourceURIValue = o.optString(getRdfKey(PROPERTY_RESOURCE));
        if (resourceURIValue != null) {
            URI uri = new URI(resourceURIValue);
            if (LyoProviderUtils.relativeURIsAreDisabled() && !uri.isAbsolute()) {
                throw new OslcCoreRelativeURIException(beanClass, "<none>", uri);
            }

            // is there also a title?
            String title = o.optString(getDctermsKey(PROPERTY_TITLE));
            if (title != null) {
                // create a Link
                return new Link(uri, title);
            }

            // create a URI
            else {
                return uri;
            }
        }

        // sequence?
        else if (RdfCollections.isSequenceSupported() && o.containsKey(getRdfKey(RdfCollections.RDF_SEQ))) {
            JSONArray sequenceMembers = o.getJSONArray(getRdfKey(RdfCollections.RDF_SEQ));
            return buildExtendedSequence(sequenceMembers, beanClass, propertyQName, rdfTypes);
        }

        // Handle an inline resource.
        else {
            AbstractResource any = new AnyResource();
            buildResource(o, AnyResource.class, any, rdfTypes);
            return any;
        }
    }

    private Object buildExtendedSequence(JSONArray sequenceMembers, Class<?> beanClass, QName propertyQName, Set<String> rdfTypes)
            throws URISyntaxException, DatatypeConfigurationException, IllegalAccessException, InstantiationException, InvocationTargetException,
            OslcCoreApplicationException, NoSuchMethodException {
        List<Object> sequence = RdfCollections.createSequence();
        JsonArray jakartaArray = sequenceMembers.build();
        for (JsonValue v : jakartaArray) {
                Object value = buildExtendedValue(v, beanClass, propertyQName, rdfTypes);
                sequence.add(value);
        }
        return sequence;
    }

    private Object buildExtendedArray(JsonArray array, Class<?> beanClass, QName propertyQName, Set<String> rdfTypes)
            throws DatatypeConfigurationException, URISyntaxException, IllegalAccessException, InstantiationException, InvocationTargetException,
            OslcCoreApplicationException, NoSuchMethodException {

        List<Object> collection = new ArrayList<>();
        for (JsonValue element : array) {
            collection.add(buildExtendedValue(element, beanClass, propertyQName, rdfTypes));
        }
        return collection;
    }

    private void fillInRdfType(JSONObject jsonObject, IExtendedResource resource)
            throws URISyntaxException, OslcCoreMissingNamespaceDeclarationException {

        String typeProperty = getRdfKey(PROPERTY_TYPE);
        if (jsonObject.has(typeProperty)) {
            JSONArray array = jsonObject.getJSONArray(typeProperty);
            for (int i = 0; i < array.size(); ++i) {
                JSONObject typeObj = array.getJSONObject(i);
                resource.addType(new URI(typeObj.getString(getRdfKey(PROPERTY_RESOURCE))));
            }
        }
    }

    private boolean isRdfListNode(PropertyAccessor accessor, JsonValue jsonValue) throws OslcCoreMissingNamespaceDeclarationException {
        if (!(jsonValue instanceof JsonObject)) {
            return false;
        }

        JSONObject jsonObject = new JSONObject((JsonObject) jsonValue);

        boolean isListNode = jsonObject.has(getRdfKey(PROPERTY_FIRST)) && jsonObject.has(getRdfKey(PROPERTY_REST));
        if (isListNode) {
            return true;
        }

        boolean isNilResource = RDF_NIL_URI.equals(jsonObject.optString(getRdfKey(PROPERTY_RESOURCE)));
        if (!isNilResource) {
            return false;
        }

        OslcRdfCollectionType collectionType = accessor.getCollectionType().orElse(null);
        if ((collectionType != null) && OslcConstants.RDF_NAMESPACE.equals(collectionType.namespaceURI()) // NOSONAR
                && RdfCollections.RDF_LIST.equals(collectionType.collectionType())) {
            return true;
        }

        return false;
    }

    private Object buildAccessorValue(Class<?> beanClass, PropertyAccessor accessor, Class<?> setMethodParameterClass,
            Class<?> setMethodComponentParameterClass, JsonValue jsonValue, Set<String> rdfTypes)
            throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException, InstantiationException,
            InvocationTargetException, OslcCoreApplicationException, URISyntaxException, NoSuchMethodException {

        // determine whether the object is a RDF collection container
        boolean isRdfContainerNode = isRdfListNode(accessor, jsonValue);
        JsonArray container = null;
        if (!isRdfContainerNode && (jsonValue instanceof JsonObject)) {
            JSONObject parent = new JSONObject((JsonObject) jsonValue);

            JSONArray c = parent.optJSONArray(getRdfKey(RdfCollections.RDF_ALT));
            if (c == null) {
                c = parent.optJSONArray(getRdfKey(RdfCollections.RDF_BAG));
            }
            if (c == null) {
                c = parent.optJSONArray(getRdfKey(RdfCollections.RDF_SEQ));
            }
            if (c != null) {
                container = c.build();
            }
            isRdfContainerNode = container != null;
        }

        // json object?
        if (!isRdfContainerNode && (jsonValue instanceof JsonObject)) {
            JSONObject nestedJSONObject = new JSONObject((JsonObject) jsonValue);

            if (!IReifiedResource.class.isAssignableFrom(setMethodComponentParameterClass)) {
                // If this is the special case for an rdf:resource?
                Object uriObject = nestedJSONObject.opt(getRdfKey(PROPERTY_RESOURCE));

                if (uriObject instanceof String) {
                    URI uri = new URI((String) uriObject);

                    if (LyoProviderUtils.relativeURIsAreDisabled() && !uri.isAbsolute()) {
                        throw new OslcCoreRelativeURIException(beanClass, accessor.getSetter().getName(), uri);
                    }

                    return uri;
                }
            }

            Object nestedBean = setMethodComponentParameterClass.getDeclaredConstructor().newInstance();
            buildResource(nestedJSONObject, setMethodComponentParameterClass, nestedBean, rdfTypes);
            return nestedBean;
        }

        // json array?
        else if ((jsonValue instanceof JsonArray) || isRdfContainerNode) {
            JsonArray jsonArray;

            if (isRdfContainerNode && (container == null)) {
                JSONArray wrapperArray = new JSONArray();

                JSONObject listNode = new JSONObject((JsonObject) jsonValue);
                while ((listNode != null) && !RDF_NIL_URI.equals(listNode.opt(getRdfKey(PROPERTY_RESOURCE)))) {
                    Object o = listNode.opt(getRdfKey(PROPERTY_FIRST));
                    wrapperArray.add(o);
                    listNode = listNode.optJSONObject(getRdfKey(PROPERTY_REST));
                }
                jsonArray = wrapperArray.build();
            } else if (isRdfContainerNode) {
                jsonArray = container;
            } else {
                jsonArray = (JsonArray) jsonValue;
            }

            // build the array/collection members
            List<Object> tempList = new ArrayList<>();
            for (JsonValue jsonArrayEntryObject : jsonArray) {
                Object parameterArrayObject = buildAccessorValue(beanClass, accessor, setMethodComponentParameterClass,
                        setMethodComponentParameterClass, jsonArrayEntryObject, rdfTypes);

                tempList.add(parameterArrayObject);
            }

            // array?
            if (setMethodParameterClass.isArray()) {
                // To support primitive arrays, we have to use Array reflection to set individual elements. We cannot use Collection.toArray.
                // Array.set will unwrap objects to their corresponding primitives.
                Object array = Array.newInstance(setMethodComponentParameterClass, jsonArray.size());

                int index = 0;
                for (Object parameterArrayObject : tempList) {
                    Array.set(array, index, parameterArrayObject);
                    index++;
                }

                return array;
            }

            // collection?
            else if (Collection.class.isAssignableFrom(setMethodParameterClass)) {
                Collection<Object> collection = RdfCollections.createCollection(setMethodParameterClass);
                collection.addAll(tempList);
                return collection;
            }

            else if (!tempList.isEmpty()) {
                // Resource is expecting a single value but a collection is defined on the Json object.
                // Log a warning and return only the first value
                OslcCoreMisusedOccursException e = new OslcCoreMisusedOccursException(beanClass, accessor.getSetter());
                LOGGER.warn(e.getMessage());
                return tempList.get(0);
            } else {
                return null;
            }
        }

        // null?
        else if ((jsonValue == null) || (JsonValue.NULL.equals(jsonValue))) {
            return buildAccessorNullValue(setMethodComponentParameterClass);
        }

        // convert from the string representation
        else if (jsonValue instanceof JsonString) {
            String stringValue = ((JsonString) jsonValue).getString();
            return buildAccessorString(stringValue, setMethodComponentParameterClass);
        }

        // convert from the string representation
        else {
            String stringValue = jsonValue.toString();
            return buildAccessorString(stringValue, setMethodComponentParameterClass);
        }
    }

    private Object buildAccessorString(String value, Class<?> type) throws DatatypeConfigurationException {
        if (String.class == type) {
            return value;
        } else if ((Boolean.class == type) || (Boolean.TYPE == type)) {
            // Cannot use Boolean.parseBoolean since it supports case-insensitive TRUE.
            if (Boolean.TRUE.toString().equals(value)) {
                return Boolean.TRUE;
            } else if (Boolean.FALSE.toString().equals(value)) {
                return Boolean.FALSE;
            } else {
                throw new IllegalArgumentException("'" + value + "' has wrong format for Boolean.");
            }
        } else if ((Byte.class == type) || (Byte.TYPE == type)) {
            return Byte.valueOf(value);
        } else if ((Short.class == type) || (Short.TYPE == type)) {
            return Short.valueOf(value);
        } else if ((Integer.class == type) || (Integer.TYPE == type)) {
            return Integer.valueOf(value);
        } else if ((Long.class == type) || (Long.TYPE == type)) {
            return Long.valueOf(value);
        } else if (BigInteger.class == type) {
            return new BigInteger(value);
        } else if ((Float.class == type) || (Float.TYPE == type)) {
            if (readSpecialNumberValues()) {
                if (POSITIVE_INF.equals(value) || "Infinity".equals(value)) {
                    return Float.POSITIVE_INFINITY;
                }
                if (NEGATIVE_INF.equals(value) || "-Infinity".equals(value)) {
                    return Float.NEGATIVE_INFINITY;
                }
                if (NOT_A_NUMBER.equals(value)) {
                    return Float.NaN;
                }
            }

            return Float.valueOf(value);
        } else if ((Double.class == type) || (Double.TYPE == type)) {
            if (readSpecialNumberValues()) {
                if (POSITIVE_INF.equals(value) || "Infinity".equals(value)) {
                    return Double.POSITIVE_INFINITY;
                }
                if (NEGATIVE_INF.equals(value) || "-Infinity".equals(value)) {
                    return Double.NEGATIVE_INFINITY;
                }
                if (NOT_A_NUMBER.equals(value)) {
                    return Double.NaN;
                }
            }

            return Double.valueOf(value);
        } else if (Date.class == type) {
            return DatatypeFactory.newInstance().newXMLGregorianCalendar(value).toGregorianCalendar().getTime();
        } else {
            return null;
        }
    }

    private Object buildAccessorNullValue(Class<?> type) {
        if ((Boolean.class == type) || (Boolean.TYPE == type)) {
            throw new IllegalArgumentException("Boolean cannot be null.");
        }

        // expected double?
        if (Double.TYPE == type) {
            if (readSpecialNumberValues()) {
                LOGGER.warn("Null double value treated as NaN.");
                return Double.NaN;
            } else {
                throw new IllegalArgumentException("Null double value not allowed. You can change this behavior by setting system property of "
                        + JsonHelper.OSLC4J_READ_SPECIAL_NUMS + " to true.");
            }
        }

        // expected float?
        if (Float.TYPE == type) {
            if (readSpecialNumberValues()) {
                LOGGER.warn("Null float value treated as NaN.");
                return Float.NaN;
            } else {
                throw new IllegalArgumentException("Null float value not allowed. You can change this behavior by setting system property of "
                        + JsonHelper.OSLC4J_READ_SPECIAL_NUMS + " to true.");
            }
        }

        // expected integral number?
        if ((Short.TYPE == type) || (Integer.TYPE == type) || (Long.TYPE == type)) {
            throw new IllegalArgumentException("Null values not allowed for type " + type);
        }

        return null;
    }

    private static boolean readSpecialNumberValues() {
        return "true".equals(System.getProperty(JsonHelper.OSLC4J_READ_SPECIAL_NUMS, "true"));
    }

    private void addMappings(JSONObject prefixes) {
        for (Entry<String, JsonValue> prefixEntry : prefixes.entrySet()) {
            String prefix = prefixEntry.getKey();
            JsonValue namespace = prefixEntry.getValue();

            if (namespace instanceof JsonString) {
                String namespaceString = ((JsonString) namespace).getString();
                getNamespaceMappings().addKnownMapping(prefix, namespaceString);
            }
        }
    }

    private String getRdfKey(String localPart) throws OslcCoreMissingNamespaceDeclarationException {
        return getExistingKey(OslcConstants.RDF_NAMESPACE, localPart, OslcConstants.RDF_NAMESPACE_PREFIX);
    }

    private String getRdfsKey(String localPart) throws OslcCoreMissingNamespaceDeclarationException {
        return getExistingKey(OslcConstants.RDFS_NAMESPACE, localPart, OslcConstants.RDFS_NAMESPACE_PREFIX);
    }

    private String getDctermsKey(String localPart) throws OslcCoreMissingNamespaceDeclarationException {
        return getExistingKey(OslcConstants.DCTERMS_NAMESPACE, localPart, OslcConstants.DCTERMS_NAMESPACE_PREFIX);
    }

    private String getOslcKey(String localPart) throws OslcCoreMissingNamespaceDeclarationException {
        return getExistingKey(OslcConstants.OSLC_CORE_NAMESPACE, localPart, OslcConstants.OSLC_CORE_NAMESPACE_PREFIX);
    }

    private String getExistingKey(String namespace, String localPart, String prefix) throws OslcCoreMissingNamespaceDeclarationException {
        if (!getNamespaceMappings().containsPrefix(prefix)) {
            throw new OslcCoreMissingNamespaceDeclarationException(namespace);
        }
        return generateKey(namespace, localPart, prefix);
    }

    private QName getExistingQName(String prefix, String localPart) throws OslcCoreMissingNamespacePrefixException {
        String namespace = getNamespaceMappings().getMappings().get(prefix);
        if (namespace == null) {
            throw new OslcCoreMissingNamespacePrefixException(prefix);
        }

        return new QName(namespace, localPart, prefix);
    }
}
