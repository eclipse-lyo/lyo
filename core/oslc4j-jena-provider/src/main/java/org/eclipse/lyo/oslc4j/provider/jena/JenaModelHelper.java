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
package org.eclipse.lyo.oslc4j.provider.jena;

import org.apache.jena.rdf.model.Container;
import org.apache.jena.rdf.model.NodeIterator;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.rdf.model.ReifiedStatement;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import org.apache.jena.rdf.model.Alt;
import org.apache.jena.rdf.model.Bag;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFList;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.RSIterator;
import org.apache.jena.rdf.model.ResIterator;
import org.apache.jena.rdf.model.Seq;
import org.apache.jena.rdf.model.SimpleSelector;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.graph.BlankNodeId;
import org.apache.jena.datatypes.DatatypeFormatException;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.datatypes.xsd.XSDDateTime;
import org.apache.jena.datatypes.xsd.impl.XMLLiteralType;
import org.apache.jena.datatypes.xsd.impl.XSDDateType;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.util.ResourceUtils;

import org.eclipse.lyo.oslc4j.core.*;
import org.eclipse.lyo.oslc4j.core.annotation.*;
import org.eclipse.lyo.oslc4j.core.exception.*;
import org.eclipse.lyo.oslc4j.core.model.*;
import org.eclipse.lyo.oslc4j.provider.jena.ordfm.ResourcePackages;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Element;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.StringWriter;
import java.lang.reflect.*;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.function.Function;

@SuppressWarnings({"unused", "WeakerAccess"})
public final class JenaModelHelper
{
    private static final String PROPERTY_TOTAL_COUNT = "totalCount";
    private static final String PROPERTY_NEXT_PAGE = "nextPage";

    private static final String RDF_TYPE_URI = OslcConstants.RDF_NAMESPACE + "type";

    private static final String RDF_LIST = "List";
    private static final String RDF_ALT	 = "Alt";
    private static final String RDF_BAG	 = "Bag";
    private static final String RDF_SEQ	 = "Seq";

    private static final String METHOD_NAME_START_GET = "get";
    private static final String METHOD_NAME_START_IS  = "is";
    private static final String METHOD_NAME_START_SET = "set";

    private static final int METHOD_NAME_START_GET_LENGTH = METHOD_NAME_START_GET.length();
    private static final int METHOD_NAME_START_IS_LENGTH  = METHOD_NAME_START_IS.length();

    private static final String GENERATED_PREFIX_START = "j.";

    private static final Logger logger = LoggerFactory.getLogger(JenaModelHelper.class);

    private JenaModelHelper()
    {
        super();
    }

    public static Model createJenaModel(final Object[] objects)
            throws DatatypeConfigurationException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException,
            OslcCoreApplicationException
    {
        return createJenaModel(null,
                null,
                null,
                objects,
                null);
    }

    static Model createJenaModel(final String descriptionAbout, final String responseInfoAbout,
            final ResponseInfo<?> responseInfo, final Object[] objects, final Map<String, Object> properties)
            throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException,
            InvocationTargetException, OslcCoreApplicationException {
        
        Instant start = Instant.now();

        final Model model = ModelFactory.createDefaultModel();

        Resource descriptionResource = null;

        if (descriptionAbout != null)
        {
            if(OSLC4JUtils.isQueryResultListAsContainer())
            {
                descriptionResource = model.createResource(descriptionAbout, RDFS.Container);
            }
            else
            {
                descriptionResource = model.createResource(descriptionAbout);
            }

            Map<IExtendedResource,Resource> visitedResources = new HashMap<>();
            handleExtendedProperties(FilteredResource.class,
                    model,
                    descriptionResource,
                    responseInfo.getContainer(),
                    properties,
                    visitedResources
            );

            if (responseInfoAbout != null)
            {
                final Resource responseInfoResource = model.createResource(
                        responseInfoAbout,
                        model.createProperty(OslcConstants.TYPE_RESPONSE_INFO)
                );

                if (responseInfo != null)
                {
                    final int totalCount = responseInfo.totalCount() == null
                            ? objects.length
                            : responseInfo.totalCount();
                    responseInfoResource.addProperty(
                            model.createProperty(OslcConstants.OSLC_CORE_NAMESPACE, PROPERTY_TOTAL_COUNT),
                            model.createTypedLiteral(totalCount)
                    );

                    if (responseInfo.nextPage() != null)
                    {
                        responseInfoResource.addProperty(
                                model.createProperty(OslcConstants.OSLC_CORE_NAMESPACE, PROPERTY_NEXT_PAGE),
                                model.createResource(responseInfo.nextPage())
                        );
                    }

                    visitedResources = new HashMap<>();
                    handleExtendedProperties(ResponseInfo.class,
                            model,
                            responseInfoResource,
                            responseInfo,
                            properties,
                            visitedResources
                    );
                }
            }
        }

        // add global namespace mappings
        final Map<String, String> namespaceMappings = new HashMap<>(OslcGlobalNamespaceProvider
                .getInstance()
                .getPrefixDefinitionMap());

        for (final Object object : objects)
        {
            handleSingleResource(descriptionResource,
                    object,
                    model,
                    namespaceMappings,
                    properties);
        }

        if (descriptionAbout != null)
        {
            // Ensure we have an rdf prefix
            ensureNamespacePrefix(OslcConstants.RDF_NAMESPACE_PREFIX,
                    OslcConstants.RDF_NAMESPACE,
                    namespaceMappings);

            // Ensure we have an rdfs prefix
            ensureNamespacePrefix(OslcConstants.RDFS_NAMESPACE_PREFIX,
                    OslcConstants.RDFS_NAMESPACE,
                    namespaceMappings);

            if (responseInfoAbout != null)
            {
                // Ensure we have an oslc prefix
                ensureNamespacePrefix(OslcConstants.OSLC_CORE_NAMESPACE_PREFIX,
                        OslcConstants.OSLC_CORE_NAMESPACE,
                        namespaceMappings);
            }
        }

        // Set the namespace prefixes
        for (final Map.Entry<String, String> namespaceMapping : namespaceMappings.entrySet())
        {
            model.setNsPrefix(namespaceMapping.getKey(),
                    namespaceMapping.getValue());
        }

        Instant finish = Instant.now();
        logger.trace("createJenaModel - Execution Duration: {} ms", Duration.between(start, finish).toMillis());
        return model;
    }

    private static void handleSingleResource(final Resource			descriptionResource,
                                             final Object				object,
                                             final Model				model,
                                             final Map<String, String> namespaceMappings,
                                             final Map<String, Object> properties)
            throws DatatypeConfigurationException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException,
            OslcCoreApplicationException
    {
        final Class<?> objectClass = object.getClass();

        // Collect the namespace prefix -> namespace mappings
        recursivelyCollectNamespaceMappings(namespaceMappings,
                objectClass);

        final Resource mainResource;

        if (object instanceof URI)
        {
            mainResource = model.createResource(((URI) object).toASCIIString());
        }
        else
        {
            URI aboutURI = null;
            if (object instanceof IResource)
            {
                aboutURI = ((IResource) object).getAbout();
            }

            if (aboutURI != null)
            {
                if (OSLC4JUtils.relativeURIsAreDisabled() && !aboutURI.isAbsolute())
                {
                    throw new OslcCoreRelativeURIException(objectClass,
                            "getAbout",
                            aboutURI);
                }

                mainResource = model.createResource(aboutURI.toString());
            }
            else
            {
                mainResource = model.createResource();
            }

            if (objectClass.getAnnotation(OslcResourceShape.class) != null)
            {
                String qualifiedName = TypeFactory.getQualifiedName(objectClass);
                if (qualifiedName != null)
                {
                    mainResource.addProperty(RDF.type, model.createResource(qualifiedName));
                }
            }

            buildResource(object,
                    objectClass,
                    model,
                    mainResource,
                    properties);
        }

        if (descriptionResource != null)
        {
            descriptionResource.addProperty(RDFS.member,
                    mainResource);
        }
    }

    /**
     * Wrapper around {@link #unmarshal(Model, Class)} to unmarshal a single resource instance.
     *
     * @throws IllegalArgumentException if anything but 1 resource was unmarshaled exactly.
     */
    public static <T> T unmarshalSingle(final Model model, Class<T> clazz)
            throws IllegalArgumentException, LyoModelException {
        final T[] ts = unmarshal(model, clazz);
        if (ts.length != 1) {
            throw new IllegalArgumentException("Model shall contain exactly 1 instance of the "
                    + "class");
        }
        return ts[0];
    }

    /**
     * Wrapper around {@link #fromJenaResource(Resource, Class)} to provide generics support and group
     * exceptions.
     *
     * @param resource Jena resource to be unmarshalled
     * @param clazz Lyo resource class/interface instance to assist reflection
     * @param <T>   Same resource class/interface to make the method generic
     *
     * @return an unmarshalled resource class instance
     *
     * @throws LyoModelException if the model cannot be unmarshalled into instances of the
     *                               provided class
     */
    @SuppressWarnings({"unchecked", "deprecation"})
    public static <T> T unmarshal(final Resource resource, Class<T> clazz) throws LyoModelException {
        try {
            return (T)JenaModelHelper.fromJenaResource(resource, clazz);
        } catch (DatatypeConfigurationException | IllegalAccessException |
                InvocationTargetException | InstantiationException | OslcCoreApplicationException
                | NoSuchMethodException | URISyntaxException e) {
            throw new LyoModelException(e);
        }
    }

    /**
     * @deprecated Use {@link #unmarshal(Resource, Class)} or {@link #unmarshalSingle(Model, Class)}
     *          instead.
     */
    @Deprecated
    public static Object fromJenaResource(final Resource resource, Class<?> beanClass)
            throws DatatypeConfigurationException, IllegalAccessException,
            IllegalArgumentException,
            InstantiationException, InvocationTargetException, OslcCoreApplicationException,
            URISyntaxException, SecurityException, NoSuchMethodException {
        ResourcePackages.mapPackage(beanClass.getPackage());
        Optional<Class<?>> mostConcreteResourceClass = ResourcePackages.getClassOf(resource, beanClass);
        if (mostConcreteResourceClass.isPresent()) {
            beanClass = mostConcreteResourceClass.get();
        }
        final Object   newInstance = beanClass.newInstance();
        final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods = new HashMap<>();
        final Map<String,Object> visitedResources = new HashMap<>();
        final HashSet<String> rdfTypes = new HashSet<>();
        fromResource(classPropertyDefinitionsToSetMethods,
                beanClass,
                newInstance,
                resource,
                visitedResources,
                rdfTypes);

        return newInstance;
    }

    /**
     * Wrapper around {@link #fromJenaModel(Model, Class)} to provide generics support and group
     * exceptions.
     *
     * @param model Jena model with RDF resources to be unmarshalled
     * @param clazz Lyo resource class instance to assist reflection
     * @param <T>   Same resource class to make the method generic
     *
     * @return an array of unmarshalled resource class instances
     *
     * @throws LyoModelException if the model cannot be unmarshalled into instances of the
     *                               provided class
     */
    @SuppressWarnings({"unchecked", "deprecation"})
    public static <T> T[] unmarshal(final Model model, Class<T> clazz) throws LyoModelException {
        try {
            final Object[] objects = JenaModelHelper.fromJenaModel(model, clazz);
            return (T[]) objects;
        } catch (DatatypeConfigurationException | IllegalAccessException |
                InvocationTargetException | InstantiationException | OslcCoreApplicationException
                | NoSuchMethodException | URISyntaxException e) {
            throw new LyoModelException(e);
        }
    }

    /**
     * @deprecated Use {@link #unmarshal(Model, Class)} instead.
     */
    @Deprecated
    public static Object[] fromJenaModel(final Model model, final Class<?> beanClass)
            throws DatatypeConfigurationException, IllegalAccessException,
            IllegalArgumentException,
            InstantiationException, InvocationTargetException, OslcCoreApplicationException,
            URISyntaxException, SecurityException, NoSuchMethodException {
        
        Instant start = Instant.now();

        final List<Object> results = new ArrayList<>();

        if (beanClass.getAnnotation(OslcResourceShape.class) != null) {
            ResIterator listSubjects;

            // Fix for defect 412755
            // keep the same behavior, i.e. use the class name to match the resource rdf:type
            if (!OSLC4JUtils.useBeanClassForParsing()) {

                final String qualifiedName = TypeFactory.getQualifiedName(beanClass);
                listSubjects = model.listSubjectsWithProperty(
                        RDF.type,
                        model.getResource(qualifiedName));
                List<Resource> resourceList = listSubjects.toList();

                createObjectResultList(beanClass, results, resourceList);
            } else {
                // get the list of subjects that have rdf:type element
                listSubjects = model.listSubjectsWithProperty(RDF.type);

                List<Resource> resourceList = new ArrayList<>();

                // iterate over the list of subjects to create a list of
                // subjects that does not contain inline resources
                while (listSubjects.hasNext()) {
                    final Resource resource = listSubjects.next();

                    // check if the current resource is not an inline resource,
                    // i.e, check if it does not have a parent node
                    SimpleSelector selector = new SimpleSelector(null, null, resource);
                    StmtIterator listStatements = model.listStatements(selector);
                    if (!listStatements.hasNext()) {
                        // the current resource is not an inline resource, it
                        // should be considered in the list of subjects
                        resourceList.add(resource);
                    }
                }

                createObjectResultList(beanClass, results, resourceList);
            }

        } else if (URI.class.equals(beanClass)) {
            StmtIterator memberIterator = model.listStatements(null, RDFS.member, (RDFNode) null);
            while (memberIterator.hasNext()) {
                Statement memberStatement = memberIterator.next();
                RDFNode memberObject = memberStatement.getObject();
                if (memberObject.isURIResource()) {
                    URI memberURI = URI.create(memberObject.asResource().getURI());
                    results.add(memberURI);
                }
            }
        }

        Instant finish = Instant.now();
        logger.trace("fromJenaModel - Execution Duration: {} ms", Duration.between(start, finish).toMillis());
        return results.toArray((Object[]) Array.newInstance(beanClass, results.size()));
    }

    /**
     * Extracts a resource from the Jena model pointed to by a Link.
     * <p>
     * Useful in case of following links of a just unmarshaled resource using {@link
     * #unmarshal(Model, Class)} or {@link #unmarshalSingle(Model, Class)}.
     *
     * @param m      Jena model
     * @param l      Link that points to resouce inside the model
     * @param rClass Lyo resource class to unmarshall the OSLC resource into.
     * @param <R>    Same class argument to make method generic
     *
     * @return An instance of a resource class
     *
     * @throws IllegalArgumentException if the link does not point a resource in the model.
     */
    public static <R extends IResource> R followLink(final Model m, final Link l,
                                                     final Class<R> rClass)
            throws IllegalArgumentException, LyoModelException {
        final R[] rs = unmarshal(m, rClass);
        for (R r : rs) {
            if (l.getValue().equals(r.getAbout())) {
                return r;
            }
        }
        throw new IllegalArgumentException("Link cannot be followed in this model");
    }

    /**
     * Skolemize all blank node by replacing them with equivalent resources with an ephemeral
     * URI of
     * form 'urn:skolem:%blankNodeId%''
     *
     * @param m Jena model to skolemize
     */
    public static void skolemize(final Model m) {
        skolemize(m, blankNodeId -> "urn:skolem:" + blankNodeId.getLabelString());
    }

    /**
     * Skolemize all blank node by replacing them with equivalent resources with a URI generated by
     * the supplied lambda function.
     * <p>
     * <p>
     * See <a href="https://www.w3.org/TR/rdf11-concepts/#section-skolemization">RDF 1.1 //
     * Replacing Blank Nodes with IRIs</a> for further guidance.
     *
     * @param m                 Jena model to skolemize
     * @param skolemUriFunction Lambda function that returns ("mints") a resource URI for a BNode.
     */
    public static void skolemize(final Model m, Function<BlankNodeId, String> skolemUriFunction) {
        final ResIterator resIterator = m.listSubjects();
        while (resIterator.hasNext()) {
            final Resource resource = resIterator.nextResource();
            if (resource != null && resource.isAnon()) {
                final String skolemURI = skolemUriFunction.apply(resource.getId().getBlankNodeId
                        ());
                ResourceUtils.renameResource(resource, skolemURI);
            }
        }
    }

    private static List<Object> createObjectResultList(Class<?> beanClass,
                                                       List<Object> results, List<Resource> listSubjects)
            throws IllegalAccessException, InstantiationException,
            DatatypeConfigurationException, InvocationTargetException,
            OslcCoreApplicationException, URISyntaxException,
            NoSuchMethodException {
        if (null != listSubjects) {
            ResourcePackages.mapPackage(beanClass.getPackage());
            final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods = new HashMap<>();
            Class<?> originalBeanClass = beanClass;
            for (final Resource resource : listSubjects) {
                beanClass = originalBeanClass;
                Optional<Class<?>> mostConcreteResourceClass = ResourcePackages.getClassOf(resource,
                        beanClass);
                if (mostConcreteResourceClass.isPresent()) {
                    beanClass = mostConcreteResourceClass.get();
                    if (!originalBeanClass.isAssignableFrom(beanClass)) {
                        continue;
                    }
                }
                final Object   newInstance = beanClass.newInstance();
                final Map<String,Object> visitedResources = new HashMap<>();
                final HashSet<String> rdfTypes = new HashSet<>();

                fromResource(classPropertyDefinitionsToSetMethods,
                        beanClass,
                        newInstance,
                        resource,
                        visitedResources,
                        rdfTypes);

                results.add(newInstance);
            }
        }

        return results;
    }

    @SuppressWarnings("unchecked")
    private static void fromResource(final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods,
                                     final Class<?>							  beanClass,
                                     final Object							  bean,
                                     final Resource							  resource,
                                     Map<String,Object>				  visitedResources,
                                     HashSet<String>					  rdfTypes)
            throws DatatypeConfigurationException,
            IllegalAccessException,
            IllegalArgumentException,
            InstantiationException,
            InvocationTargetException,
            OslcCoreApplicationException,
            URISyntaxException,
            SecurityException,
            NoSuchMethodException
    {
        Map<String, Method> setMethodMap = classPropertyDefinitionsToSetMethods.get(beanClass);
        if (setMethodMap == null)
        {
            setMethodMap = createPropertyDefinitionToSetMethods(beanClass);

            classPropertyDefinitionsToSetMethods.put(beanClass,
                    setMethodMap);
        }

        visitedResources.put(getVisitedResourceName(resource),bean);

        if (bean instanceof IResource)
        {
            final String aboutURIString = resource.getURI();
            if (aboutURIString != null)
            {
                final URI aboutURI = new URI(aboutURIString);

                if (aboutURI.isAbsolute())
                {
                    ((IResource) bean).setAbout(aboutURI);
                }
                //ignore relative URIs when creating new non-local resources
                else if (!(bean instanceof AbstractResource))
                {
                    throw new OslcCoreRelativeURIException(beanClass,
                            "setAbout",
                            aboutURI);
                }


            }
        }

        // Collect values for array properties. We do this since values for
        // arrays are not required to be contiguous.
        final Map<String, List<Object>> propertyDefinitionsToArrayValues = new HashMap<>();

        // Ensure a single-value property is not set more than once
        final Set<Method> singleValueMethodsUsed = new HashSet<>();

        final StmtIterator listProperties = resource.listProperties();

        final IExtendedResource extendedResource;
        final Map<QName, Object> extendedProperties;
        if (bean instanceof IExtendedResource)
        {
            extendedResource = (IExtendedResource) bean;
            extendedProperties = new HashMap<>();
            extendedResource.setExtendedProperties(extendedProperties);
        }
        else
        {
            extendedResource = null;
            extendedProperties = null;
        }

        // get the list of resource rdf type
        rdfTypes = getTypesFromResource(resource, rdfTypes);

        while (listProperties.hasNext())
        {

            final Statement statement = listProperties.next();
            final Property	predicate = statement.getPredicate();
            final RDFNode	object	  = statement.getObject();
            final String uri	   = predicate.getURI();
            final Method setMethod = setMethodMap.get(uri);

            if (setMethod == null)
            {
                if (RDF_TYPE_URI.equals(uri))
                {
                    if (extendedResource != null)
                    {
                        final URI type = new URI(object.asResource().getURI());
                        extendedResource.addType(type);
                    }
                    // Otherwise ignore missing propertyDefinition for rdf:type.

                }
                else
                {
                    if (extendedProperties == null)
                    {
                        logger.debug("Set method not found for object type:	" +
                                beanClass.getName() +
                                ", uri:	" +
                                uri);
                    }
                    else
                    {

                        String prefix = resource.getModel().getNsURIPrefix(predicate.getNameSpace());
                        if (prefix == null)
                        {
                            prefix = generatePrefix(resource.getModel(), predicate.getNameSpace());
                        }
                        final QName key = new QName(predicate.getNameSpace(),
                                predicate.getLocalName(), prefix);
                        final Object value = handleExtendedPropertyValue(beanClass, object,
                                visitedResources, key, rdfTypes);
                        final Object previous = extendedProperties.get(key);

                        if (previous == null)
                        {
                            extendedProperties.put(key, value);
                        }
                        else
                        {
                            final Collection<Object> collection;
                            if (previous instanceof Collection)
                            {
                                collection = ((Collection<Object>) previous);
                            }
                            else
                            {
                                collection = new ArrayList<>();
                                collection.add(previous);
                                extendedProperties.put(key, collection);
                            }
                            collection.add(value);
                        }
                    }
                }
            }
            else
            {

                Class<?> setMethodComponentParameterClass = setMethod.getParameterTypes()[0];

                boolean multiple = setMethodComponentParameterClass.isArray();

                if (multiple)
                {
                    setMethodComponentParameterClass = setMethodComponentParameterClass.getComponentType();
                }
                else if (Collection.class.isAssignableFrom(setMethodComponentParameterClass))
                {
                    multiple = true;

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

                final List<RDFNode> objects;
                if (multiple && object.isResource() && (
                        (object.asResource().hasProperty(RDF.first) && object.asResource()
                                .hasProperty(RDF.rest))
                                || (RDF.nil.equals(object)) || object.canAs(RDFList.class))) {
                    objects = new ArrayList<>();
                    Resource listNode = object.asResource();
                    while (listNode != null && !RDF.nil.getURI().equals(listNode.getURI())) {
                        visitedResources.put(getVisitedResourceName(listNode), new Object());

                        RDFNode o = listNode.getPropertyResourceValue(RDF.first);
                        objects.add(o);

                        listNode = listNode.getPropertyResourceValue(RDF.rest);
                    }

                    visitedResources.put(getVisitedResourceName(object.asResource()), objects);
                } else {
                    final Class<? extends Container> collectionResourceClass =
                            getRdfCollectionResourceClass(
                                    object.getModel(),
                                    object);
                    if (multiple && collectionResourceClass != null) {
                        objects = new ArrayList<>();
                        Container container = object.as(collectionResourceClass);
                        NodeIterator iterator = container.iterator();
                        while (iterator.hasNext()) {
                            RDFNode o = iterator.next();

                            if (o.isResource()) {
                                visitedResources.put(getVisitedResourceName(o.asResource()),
                                        new Object());
                            }

                            objects.add(o);
                        }

                        visitedResources.put(getVisitedResourceName(object.asResource()), objects);
                    } else {
                        objects = Collections.singletonList(object);
                    }
                }

                Class<?> reifiedClass = null;
                if (IReifiedResource.class.isAssignableFrom(setMethodComponentParameterClass))
                {
                    reifiedClass = setMethodComponentParameterClass;
                    final Type genericType = setMethodComponentParameterClass.getGenericSuperclass();

                    if (genericType instanceof ParameterizedType)
                    {
                        final ParameterizedType parameterizedType = (ParameterizedType) genericType;
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

                for (RDFNode o : objects)
                {
                    Object parameter = null;
                    if (o.isLiteral())
                    {
                        final Literal literal	 = o.asLiteral();
                        final String stringValue = literal.getString();

                        if (String.class == setMethodComponentParameterClass)
                        {
                            parameter = stringValue;
                        }
                        else if ((Boolean.class == setMethodComponentParameterClass) ||
                                (Boolean.TYPE == setMethodComponentParameterClass))
                        {
                            // XML supports both 'true' and '1' for a true Boolean.
                            // Cannot use Boolean.parseBoolean since it supports case-insensitive TRUE.
                            if ((Boolean.TRUE.toString().equals(stringValue)) ||
                                    ("1".equals(stringValue)))
                            {
                                parameter = Boolean.TRUE;
                            }
                            // XML supports both 'false' and '0' for a false Boolean.
                            else if ((Boolean.FALSE.toString().equals(stringValue)) ||
                                    ("0".equals(stringValue)))
                            {
                                parameter = Boolean.FALSE;
                            }
                            else
                            {
                                throw new IllegalArgumentException("'" + stringValue +
                                        "' has wrong format for Boolean.");
                            }
                        }
                        else if ((Byte.class == setMethodComponentParameterClass) ||
                                (Byte.TYPE == setMethodComponentParameterClass))
                        {
                            parameter = Byte.valueOf(stringValue);
                        }
                        else if ((Short.class == setMethodComponentParameterClass) ||
                                (Short.TYPE == setMethodComponentParameterClass))
                        {
                            parameter = Short.valueOf(stringValue);
                        }
                        else if ((Integer.class == setMethodComponentParameterClass) ||
                                (Integer.TYPE == setMethodComponentParameterClass))
                        {
                            parameter = Integer.valueOf(stringValue);
                        }
                        else if ((Long.class == setMethodComponentParameterClass) ||
                                (Long.TYPE == setMethodComponentParameterClass))
                        {
                            parameter = Long.valueOf(stringValue);
                        }
                        else if (BigInteger.class == setMethodComponentParameterClass)
                        {
                            parameter = new BigInteger(stringValue);
                        }
                        else if ((Float.class == setMethodComponentParameterClass) ||
                                (Float.TYPE == setMethodComponentParameterClass))
                        {
                            parameter = XSDDatatype.XSDfloat.parseValidated(stringValue);
                        }
                        else if ((Double.class == setMethodComponentParameterClass) ||
                                (Double.TYPE == setMethodComponentParameterClass))
                        {
                            parameter = XSDDatatype.XSDdouble.parseValidated(stringValue);
                        }
                        else if (Date.class == setMethodComponentParameterClass)
                        {
                            parameter = DatatypeFactory.newInstance()
                                    .newXMLGregorianCalendar(stringValue)
                                    .toGregorianCalendar()
                                    .getTime();
                        }
                    }
                    else if (o.isResource())
                    {
                        final Resource nestedResource = o.asResource();

                        if (URI.class == setMethodComponentParameterClass)
                        {
                            final String nestedResourceURIString = nestedResource.getURI();

                            if (nestedResourceURIString != null)
                            {
                                final URI nestedResourceURI = new URI(nestedResourceURIString);

                                if (OSLC4JUtils.relativeURIsAreDisabled() && !nestedResourceURI.isAbsolute())
                                {
                                    throw new OslcCoreRelativeURIException(beanClass,
                                            setMethod.getName(),
                                            nestedResourceURI);
                                }

                                parameter = nestedResourceURI;
                            }
                        }
                        else
                        {
                            Optional<Class<?>> optionalResourceClass = ResourcePackages.getClassOf(
                                    nestedResource, setMethodComponentParameterClass);
                            Class<?> resourceClass = optionalResourceClass.isPresent()
                                    ? optionalResourceClass.get()
                                    : setMethodComponentParameterClass;
                            final Object nestedBean = resourceClass.newInstance();
                            fromResource(classPropertyDefinitionsToSetMethods,
                                    nestedBean.getClass(),
                                    nestedBean,
                                    nestedResource,
                                    visitedResources,
                                    rdfTypes);

                            parameter = nestedBean;
                        }
                    }

                    if (parameter != null)
                    {
                        if (reifiedClass != null)
                        {
                            // This property supports reified statements. Create the
                            // new resource to hold the value and any metadata.
                            final Object reifiedResource = reifiedClass.newInstance();

                            // Find a setter for the actual value.
                            for (Method method : reifiedClass.getMethods())
                            {
                                if (!"setValue".equals(method.getName()))
                                {
                                    continue;
                                }
                                final Class<?>[] parameterTypes = method.getParameterTypes();
                                if (parameterTypes.length == 1
                                        && parameterTypes[0].isAssignableFrom(setMethodComponentParameterClass))
                                {
                                    method.invoke(reifiedResource, parameter);
                                    break;
                                }
                            }

                            // Fill in any reified statements.
                            RSIterator rsIter = statement.listReifiedStatements();
                            while (rsIter.hasNext())
                            {
                                ReifiedStatement reifiedStatement = rsIter.next();
                                fromResource(classPropertyDefinitionsToSetMethods,
                                        reifiedClass,
                                        reifiedResource,
                                        reifiedStatement,
                                        visitedResources,
                                        rdfTypes);
                            }

                            parameter = reifiedResource;
                        }

                        if (multiple)
                        {
                            List<Object> values = propertyDefinitionsToArrayValues.get(uri);
                            if (values == null)
                            {
                                values = new ArrayList<>();

                                propertyDefinitionsToArrayValues.put(uri,
                                        values);
                            }

                            values.add(parameter);
                        }
                        else
                        {
                            if (singleValueMethodsUsed.contains(setMethod))
                            {
                                throw new OslcCoreMisusedOccursException(beanClass,
                                        setMethod);
                            }


                            setMethod.invoke(bean,
                                    parameter);

                            singleValueMethodsUsed.add(setMethod);
                        }
                    }
                }
            }
        }

        // Now, handle array and collection values since all are collected.
        for (final Map.Entry<String, List<Object>> propertyDefinitionToArrayValues
                : propertyDefinitionsToArrayValues.entrySet())
        {
            final String	   uri			  = propertyDefinitionToArrayValues.getKey();
            final List<Object> values		  = propertyDefinitionToArrayValues.getValue();
            final Method	   setMethod	  = setMethodMap.get(uri);
            final Class<?>	   parameterClass = setMethod.getParameterTypes()[0];

            if (parameterClass.isArray())
            {
                final Class<?> setMethodComponentParameterClass = parameterClass.getComponentType();

                // To support primitive arrays, we have to use Array reflection to
                // set individual elements. We cannot use Collection.toArray.
                // Array.set will unwrap objects to their corresponding primitives.
                final Object array = Array.newInstance(setMethodComponentParameterClass,
                        values.size());

                int index = 0;
                for (final Object value : values)
                {
                    Array.set(array,
                            index++,
                            value);
                }

                setMethod.invoke(bean,
                        array);
            }
            // Else - we are dealing with a collection or a subclass of collection
            else
            {
                final Collection<Object> collection;

                // Handle the Collection, List, Deque, Queue interfaces.
                // Handle the AbstractCollection, AbstractList, AbstractSequentialList classes
                if ((Collection.class			  == parameterClass) ||
                        (List.class					  == parameterClass) ||
                        (Deque.class				  == parameterClass) ||
                        (Queue.class				  == parameterClass) ||
                        (AbstractCollection.class	  == parameterClass) ||
                        (AbstractList.class			  == parameterClass) ||
                        (AbstractSequentialList.class == parameterClass))
                {
                    collection = new LinkedList<>();
                }
                // Handle the Set interface
                // Handle the AbstractSet class
                else if ((Set.class			 == parameterClass) ||
                        (AbstractSet.class	 == parameterClass))
                {
                    collection = new HashSet<>();
                }
                // Handle the SortedSet and NavigableSet interfaces
                else if ((SortedSet.class	 == parameterClass) ||
                        (NavigableSet.class == parameterClass)) {
                    collection = new TreeSet<>();
                }
                // Not handled above.  Let's try newInstance with possible failure.
                else
                {
                    collection = ((Collection<Object>) parameterClass.newInstance());
                }

                collection.addAll(values);

                setMethod.invoke(bean,
                        collection);
            }
        }
    }

    /**
     * Returns a hash set of rdf:types for a given resource object. If the set
     * was populated before, returns the given list. This list will only be
     * populated if the property inferTypeFromShape is set to true.
     *
     * @param resource
     * @param types
     * @return List of rdf:types
     */
    private static HashSet<String> getTypesFromResource(final Resource resource, HashSet<String> types) {
        // The list of rdf:types will be populated only if the property
        // inferTypeFromShape is set and if the list was not populated before.
        // This is necessary because for an inline resource, the retuned
        // rdf:type is not from the parent resource, it is from the actual
        // resource.
        if (OSLC4JUtils.inferTypeFromShape() && types.isEmpty()) {
            StmtIterator rdfTypesIterator = resource.listProperties(RDF.type);
            while (rdfTypesIterator.hasNext()) {
                Statement rdfTypeStmt = rdfTypesIterator.next();
                RDFNode object = rdfTypeStmt.getObject();
                if (object.isResource()) {
                    String rdfType = object.asResource().getURI();
                    types.add(rdfType);
                }
            }
        }
        return types;
    }

    private static Class<? extends Container> getRdfCollectionResourceClass(Model model, RDFNode object)
    {
        if (object.isResource())
        {
            Resource resource = object.asResource();
            if (resource.hasProperty(RDF.type, model.getResource(OslcConstants.RDF_NAMESPACE + RDF_ALT)))
                return Alt.class;
            if (resource.hasProperty(RDF.type, model.getResource(OslcConstants.RDF_NAMESPACE + RDF_BAG)))
                return Bag.class;
            if (resource.hasProperty(RDF.type, model.getResource(OslcConstants.RDF_NAMESPACE + RDF_SEQ)))
                return Seq.class;
        }
        return null;
    }

    /**
     * Generates a prefix for unrecognized namespaces when reading in unknown
     * properties and content.
     *
     * @param model
     *			  the model
     * @param namespace
     *			  the unrecognized namespace URI that needs a prefix
     * @return the generated prefix (e.g., 'j.0')
     */
    private static String generatePrefix(Model model, String namespace)
    {
        final Map<String, String> map = model.getNsPrefixMap();
        int i = 0;
        String candidatePrefix;
        do {
            candidatePrefix = GENERATED_PREFIX_START + i;
            ++i;
        } while (map.containsKey(candidatePrefix));

        model.setNsPrefix(candidatePrefix, namespace);
        return candidatePrefix;
    }

    private static Object handleExtendedPropertyValue(final Class<?> beanClass,
                                                      final RDFNode object,
                                                      Map<String,Object> visitedResources,
                                                      final QName propertyQName,
                                                      final HashSet<String> rdfTypes)
            throws URISyntaxException,
            IllegalArgumentException,
            SecurityException,
            DatatypeConfigurationException,
            IllegalAccessException,
            InstantiationException,
            InvocationTargetException,
            OslcCoreApplicationException,
            NoSuchMethodException

    {
        if (object.isLiteral())
        {

            final Literal literal = object.asLiteral();
            // fix for Bug 412789
            if (OSLC4JUtils.inferTypeFromShape()) {

                // get property data type
                RDFDatatype dataType = literal.getDatatype();

                // infer the data type from the Resource Shape only if the
                // data type was not explicit passed in the original request
                if (null == dataType) {
                    Object newObject = OSLC4JUtils.getValueBasedOnResourceShapeType(rdfTypes,
                            propertyQName, literal.getString());

                    // return the value only if the type was really inferred
                    // from the resource shape, otherwise keep the same
                    // behavior, i.e., return a String value
                    if (null != newObject) {
                        return newObject;
                    }
                }
            }
            final Object literalValue;
            try
            {
                literalValue = literal.getValue();

            }
            catch (DatatypeFormatException e)
            {
                String rawValue = literal.getString();
                String datatype = literal.getDatatypeURI();

                if ("false".equals(System.getProperty(AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES)))
                {
                    if (logger.isWarnEnabled())
                    {
                        logger.warn(
                                "Property " + propertyQName.getNamespaceURI()
                                        + propertyQName.getLocalPart()
                                        + " could not be parsed as datatype "
                                        + literal.getDatatypeURI(), e);
                    }

                    return new UnparseableLiteral(rawValue,datatype);
                }
                else
                {
                    throw e;
                }
            }

            if (literalValue instanceof XSDDateTime)
            {
                final XSDDateTime xsdDateTime = (XSDDateTime) literalValue;
                return xsdDateTime.asCalendar().getTime();
            }

            if (XMLLiteralType.theXMLLiteralType.getURI().equals(literal.getDatatypeURI()))
            {
                return new XMLLiteral(literal.getString());
            }

            if (XSDDatatype.XSDdecimal.getURI().equals(literal.getDatatypeURI())) {
                return new BigDecimal(literal.getString());
            }

            return literalValue;
        }

        final Resource nestedResource = object.asResource();

        // Is this an inline resource? AND we have not visited it yet?
        if ((nestedResource.getURI() == null || nestedResource.listProperties().hasNext()) &&
                (!visitedResources.containsKey(getVisitedResourceName(nestedResource))))
        {
            final AbstractResource any = new AnyResource();
            final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods = new HashMap<>();
            fromResource(classPropertyDefinitionsToSetMethods,
                    AnyResource.class,
                    any,
                    nestedResource,
                    visitedResources,
                    rdfTypes);

            return any;
        }

        if (nestedResource.getURI() == null || nestedResource.listProperties().hasNext())
        {
            return visitedResources.get(getVisitedResourceName(nestedResource));
        } else {
            // It's a resource reference.
            final URI nestedResourceURI = new URI(nestedResource.getURI());
            if (OSLC4JUtils.relativeURIsAreDisabled() && !nestedResourceURI.isAbsolute())
            {
                throw new OslcCoreRelativeURIException(beanClass, "<none>",
                        nestedResourceURI);
            }

            return nestedResourceURI;
        }
    }

    private static Map<String, Method> createPropertyDefinitionToSetMethods(final Class<?> beanClass)
            throws OslcCoreApplicationException
    {
        final Map<String, Method> result = new HashMap<>();

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
                    final OslcPropertyDefinition oslcPropertyDefinitionAnnotation =
                            InheritedMethodAnnotationHelper.getAnnotation(method,
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

    private static void buildResource(final Object	 object,
                                      final Class<?> resourceClass,
                                      final Model	 model,
                                      final Resource mainResource,
                                      final Map<String, Object> properties)
            throws DatatypeConfigurationException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException,
            OslcCoreApplicationException
    {
        if (properties == OSLC4JConstants.OSL4J_PROPERTY_SINGLETON)
        {
            return;
        }

        for (final Method method : resourceClass.getMethods())
        {
            if (method.getParameterTypes().length == 0)
            {
                final String methodName = method.getName();

                if (((methodName.startsWith(METHOD_NAME_START_GET)) &&
                        (methodName.length() > METHOD_NAME_START_GET_LENGTH)) ||
                        ((methodName.startsWith(METHOD_NAME_START_IS)) &&
                                (methodName.length() > METHOD_NAME_START_IS_LENGTH)))
                {
                    final OslcPropertyDefinition oslcPropertyDefinitionAnnotation =
                            InheritedMethodAnnotationHelper.getAnnotation(method,
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
                                final Map<String, Object> map = (Map<String, Object>)properties
                                        .get(oslcPropertyDefinitionAnnotation.value());

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

                            buildAttributeResource(resourceClass,
                                    method,
                                    oslcPropertyDefinitionAnnotation,
                                    model,
                                    mainResource,
                                    value,
                                    nestedProperties,
                                    onlyNested);
                        }
                    }
                }
            }
        }

        // Handle any extended properties.
        if (object instanceof IExtendedResource)
        {
            final IExtendedResource extendedResource = (IExtendedResource) object;
            Map<IExtendedResource,Resource> visitedResources = new HashMap<>();
            handleExtendedProperties(resourceClass,
                    model,
                    mainResource,
                    extendedResource,
                    properties,
                    visitedResources);
        }
    }

    protected static void handleExtendedProperties(final Class<?> resourceClass,
                                                   final Model model,
                                                   final Resource mainResource,
                                                   final IExtendedResource extendedResource,
                                                   final Map<String, Object> properties,
                                                   Map<IExtendedResource, Resource> visitedResources)
            throws DatatypeConfigurationException,
            IllegalAccessException,
            InvocationTargetException,
            OslcCoreApplicationException
    {
        visitedResources.put(extendedResource,mainResource);
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

            final Resource typeResource = model.createResource(propertyName);

            if (!mainResource.hasProperty(RDF.type, typeResource))
            {
                mainResource.addProperty(RDF.type, typeResource);
            }
        }

        final Transformer transformer = createTransformer();

        for (final Map.Entry<QName, ?> extendedProperty : extendedResource.getExtendedProperties().entrySet())
        {
            final QName qName = extendedProperty.getKey();
            final String propertyName = qName.getNamespaceURI() + qName.getLocalPart();
            Map<String, Object> nestedProperties = null;
            boolean onlyNested = false;

            if (properties != null)
            {
                @SuppressWarnings("unchecked")
                final Map<String, Object> map = (Map<String, Object>)properties.get(propertyName);

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

            final Property property = model.createProperty(propertyName);
            final Object value = extendedProperty.getValue();

            if (value instanceof Collection)
            {
                final Collection<?> collection = (Collection<?>) value;
                for (Object next : collection)
                {
                    handleExtendedValue(resourceClass,
                            next,
                            model,
                            mainResource,
                            property,
                            nestedProperties,
                            onlyNested,
                            visitedResources,
                            transformer);
                }
            }
            else
            {
                handleExtendedValue(resourceClass,
                        value,
                        model,
                        mainResource,
                        property,
                        nestedProperties,
                        onlyNested,
                        visitedResources,
                        transformer);
            }
        }
    }

    private static void handleExtendedValue(final Class<?> objectClass,
                                            final Object value,
                                            final Model model,
                                            final Resource resource,
                                            final Property property,
                                            final Map<String, Object> nestedProperties,
                                            final boolean onlyNested,
                                            final Map<IExtendedResource, Resource> visitedResources,
                                            final Transformer transformer)
            throws DatatypeConfigurationException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException,
            OslcCoreApplicationException
    {
        if (value instanceof UnparseableLiteral)
        {
            if (onlyNested)
            {
                return;
            }

            final UnparseableLiteral unparseable = (UnparseableLiteral) value;
            resource.addProperty(property, model.createLiteral(unparseable.getRawValue()));
        }
        else if (value instanceof AnyResource)
        {
            final AbstractResource any = (AbstractResource) value;

            //create a new resource in the model if we have not visited it yet
            if (!visitedResources.containsKey(any))
            {
                final Resource nestedResource;
                final URI aboutURI = any.getAbout();
                if (aboutURI != null)
                {
                    if (OSLC4JUtils.relativeURIsAreDisabled() && !aboutURI.isAbsolute())
                    {
                        throw new OslcCoreRelativeURIException(AnyResource.class,
                                "getAbout",
                                aboutURI);
                    }

                    nestedResource = model.createResource(aboutURI.toString());
                }
                else
                {
                    nestedResource = model.createResource();
                }

                for (final URI type : any.getTypes())
                {
                    final String propertyName = type.toString();

                    if (nestedProperties == null ||
                            nestedProperties.get(propertyName) != null ||
                            nestedProperties instanceof NestedWildcardProperties ||
                            nestedProperties instanceof SingletonWildcardProperties)
                    {
                        nestedResource.addProperty(RDF.type, model.createResource(propertyName));
                    }
                }

                handleExtendedProperties(AnyResource.class, model, nestedResource, any,
                        nestedProperties, visitedResources);
                resource.addProperty(property, nestedResource);
            } else {
                //We've already added the inline resource, add a reference to it for this property
                resource.addProperty(property, visitedResources.get(any));
            }

        }
        else if (value.getClass().getAnnotation(OslcResourceShape.class) != null || value instanceof URI)
        {
            //TODO:	 Until we handle XMLLiteral for incoming unknown resources, need to assume it is
            // not XMLLiteral
            boolean xmlliteral = false;
            handleLocalResource(objectClass,
                    null,
                    xmlliteral,
                    value,
                    model,
                    resource,
                    property,
                    nestedProperties,
                    onlyNested,
                    null);
        }
        else if (value instanceof Date)
        {
            if (onlyNested)
            {
                return;
            }

            final Calendar cal = Calendar.getInstance();
            cal.setTime((Date) value);
            RDFDatatype dataType = null;

            // Check if it is a Date field
            if ( OSLC4JUtils.inferTypeFromShape() ) {
                // get the list of resource rdf type
                HashSet<String> rdfTypes = new HashSet<>();
                rdfTypes = getTypesFromResource(resource, rdfTypes);
                dataType = OSLC4JUtils.getDataTypeBasedOnResourceShapeType(rdfTypes, property);
            }

            if ( dataType != null && dataType instanceof XSDDateType) {
                XSDDateTime valuec = new XSDDateTime( cal);
                valuec.narrowType(XSDDatatype.XSDdate);
                String valueDate = valuec.toString();
                if ( valueDate != null && valueDate.endsWith("Z")){
                    valueDate = valueDate.replaceAll("Z","");
                }
                resource.addProperty(property, model.createTypedLiteral(valueDate, XSDDatatype.XSDdate));
            } else {
                resource.addProperty(property, model.createTypedLiteral(cal));
            }

        }
        else if (value instanceof XMLLiteral)
        {
            final XMLLiteral xmlLiteral = (XMLLiteral) value;
            final Literal xmlString = model.createTypedLiteral(xmlLiteral.getValue(),
                    XMLLiteralType.theXMLLiteralType);

            resource.addProperty(property, xmlString);
        }
        else if (value instanceof Element)
        {
            final StreamResult result = new StreamResult(new StringWriter());

            final DOMSource source = new DOMSource((Element)value);

            try
            {
                transformer.transform(source, result);
            } catch (TransformerException e) {
                throw new RuntimeException(e);
            }

            final Literal xmlString = model.createLiteral(result.getWriter().toString(), true);

            resource.addProperty(property, xmlString);
        }
        else if (value instanceof String)
        {
            if (onlyNested)
            {
                return;
            }

            resource.addProperty(property, model.createLiteral((String) value));
        }
        else if (value instanceof Float)
        {
            if (onlyNested)
            {
                return;
            }

            final Float f = (Float) value;
            final Literal l = toLiteral(model, f);
            resource.addProperty(property, l);
        }
        else if (value instanceof Double)
        {
            if (onlyNested)
            {
                return;
            }

            final Double d = (Double) value;
            final Literal l = toLiteral(model, d);
            resource.addProperty(property, l);
        }
        else
        {
            if (onlyNested)
            {
                return;
            }

            resource.addProperty(property, model.createTypedLiteral(value));
        }
    }

    private static Literal toLiteral(final Model model, final Float f)
    {
        if (f.compareTo(Float.POSITIVE_INFINITY) == 0)
        {
            logger.warn("+INF float is serialised to the model");
            return model.createTypedLiteral("INF", XSDDatatype.XSDfloat.getURI());
        }
        else if (f.compareTo(Float.NEGATIVE_INFINITY) == 0)
        {
            logger.warn("-INF float is serialised to the model");
            return model.createTypedLiteral("-INF", XSDDatatype.XSDfloat.getURI());
        }

        return model.createTypedLiteral(f, XSDDatatype.XSDfloat);
    }

    private static Literal toLiteral(final Model model, final Double d)
    {
        if (d.compareTo(Double.POSITIVE_INFINITY) == 0)
        {
            logger.warn("+INF double is serialised to the model");
            return model.createTypedLiteral("INF", XSDDatatype.XSDdouble.getURI());
        }
        else if (d.compareTo(Double.NEGATIVE_INFINITY) == 0)
        {
            logger.warn("-INF float is serialised to the model");
            return model.createTypedLiteral("-INF", XSDDatatype.XSDdouble.getURI());
        }

        return model.createTypedLiteral(d, XSDDatatype.XSDdouble);
    }

    private static void buildAttributeResource(final Class<?>				resourceClass,
                                               final Method					method,
                                               final OslcPropertyDefinition propertyDefinitionAnnotation,
                                               final Model					model,
                                               Resource						resource,
                                               final Object					value,
                                               final Map<String, Object>	nestedProperties,
                                               final boolean				onlyNested)
            throws DatatypeConfigurationException,
            IllegalAccessException,
            IllegalArgumentException,
            InvocationTargetException,
            OslcCoreApplicationException
    {
        final String propertyDefinition = propertyDefinitionAnnotation.value();

        final OslcName nameAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method,
                OslcName.class);

        final String name;
        if (nameAnnotation != null)
        {
            name = nameAnnotation.value();
        } else {
            name = getDefaultPropertyName(method);
        }

        if (!propertyDefinition.endsWith(name)) {
            throw new OslcCoreInvalidPropertyDefinitionException(resourceClass,
                    method,
                    propertyDefinitionAnnotation
            );
        }

        final OslcValueType valueTypeAnnotation = InheritedMethodAnnotationHelper
                .getAnnotation(method, OslcValueType.class);

        final boolean xmlLiteral = valueTypeAnnotation != null && ValueType.XMLLiteral.equals(
                valueTypeAnnotation.value());

        final Property attribute = model.createProperty(propertyDefinition);

        final Class<?> returnType = method.getReturnType();
        final OslcRdfCollectionType collectionType =
                InheritedMethodAnnotationHelper.getAnnotation(method,
                        OslcRdfCollectionType.class);
        final List<RDFNode> rdfNodeContainer;

        if (collectionType != null &&
                OslcConstants.RDF_NAMESPACE.equals(collectionType.namespaceURI()) &&
                (RDF_LIST.equals(collectionType.collectionType())
                        || RDF_ALT.equals(collectionType.collectionType())
                        || RDF_BAG.equals(collectionType.collectionType())
                        || RDF_SEQ.equals(collectionType.collectionType())))
        {
            rdfNodeContainer = new ArrayList<>();
        }
        else
        {
            rdfNodeContainer = null;
        }

        if (returnType.isArray())
        {
            // We cannot cast to Object[] in case this is an array of
            // primitives. We will use Array reflection instead.
            // Strange case about primitive arrays: they cannot be cast to
            // Object[], but retrieving their individual elements
            // via Array.get does not return primitives, but the primitive
            // object wrapping counterparts like Integer, Byte, Double, etc.
            final int length = Array.getLength(value);

            for (int index = 0;
                 index < length;
                 index++)
            {
                final Object object = Array.get(value,
                        index);

                handleLocalResource(resourceClass,
                        method,
                        xmlLiteral,
                        object,
                        model,
                        resource,
                        attribute,
                        nestedProperties,
                        onlyNested,
                        rdfNodeContainer);
            }

            if (rdfNodeContainer != null)
            {
                RDFNode container = createRdfContainer(collectionType,
                        rdfNodeContainer,
                        model);
                Statement s = model.createStatement(resource, attribute, container);

                model.add(s);
            }
        }
        else if (Collection.class.isAssignableFrom(returnType))
        {
            @SuppressWarnings("unchecked")
            final Collection<Object> collection = (Collection<Object>) value;

            for (final Object object : collection)
            {
                handleLocalResource(resourceClass,
                        method,
                        xmlLiteral,
                        object,
                        model,
                        resource,
                        attribute,
                        nestedProperties,
                        onlyNested,
                        rdfNodeContainer);
            }

            if (rdfNodeContainer != null)
            {
                RDFNode container = createRdfContainer(collectionType,
                        rdfNodeContainer,
                        model);
                Statement s = model.createStatement(resource, attribute, container);

                model.add(s);
            }
        }
        else
        {
            handleLocalResource(resourceClass,
                    method,
                    xmlLiteral,
                    value,
                    model,
                    resource,
                    attribute,
                    nestedProperties,
                    onlyNested,
                    null);
        }
    }

    private static RDFNode createRdfContainer(final OslcRdfCollectionType collectionType,
                                              final List<RDFNode> rdfNodeContainer, final Model model) {
        if (RDF_LIST.equals(collectionType.collectionType())) {
            return model.createList(rdfNodeContainer.iterator());
        }

        Container container;
        if (RDF_ALT.equals(collectionType.collectionType())) {
            container = model.createAlt();
        } else if (RDF_BAG.equals(collectionType.collectionType())) {
            container = model.createBag();
        } else {
            container = model.createSeq();
        }

        for (RDFNode node : rdfNodeContainer) {
            container.add(node);
        }

        return container;
    }

    private static void handleLocalResource(final Class<?>			  resourceClass,
                                            final Method			  method,
                                            final boolean			  xmlLiteral,
                                            final Object			  object,
                                            final Model				  model,
                                            final Resource			  resource,
                                            final Property attribute, final Map<String, Object> nestedProperties,
                                            final boolean onlyNested,
                                            final List<RDFNode> rdfNodeContainer)
            throws DatatypeConfigurationException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            OslcCoreApplicationException
    {
        if (object == null)
        {
            return;
        }
        final Class<?> objectClass = object.getClass();

        RDFNode nestedNode = null;
        final IReifiedResource<?> reifiedResource = (object instanceof IReifiedResource)
                ? (IReifiedResource<?>) object
                : null;
        final Object value = (reifiedResource == null) ? object : reifiedResource.getValue();
        if (value == null)
        {
            return;
        }

        if (value instanceof String)
        {
            if (onlyNested)
            {
                return;
            }

            if (xmlLiteral)
            {
                nestedNode = model.createTypedLiteral(value.toString(),
                        XMLLiteralType.theXMLLiteralType);
            }
            else
            {
                nestedNode = model.createLiteral(value.toString());
            }
        }
        // Floats need special handling for infinite values.
        else if (value instanceof Float)
        {
            if (onlyNested)
            {
                return;
            }

            nestedNode = toLiteral(model, (Float) value);
        }
        // Doubles need special handling for infinite values.
        else if (value instanceof Double)
        {
            if (onlyNested)
            {
                return;
            }

            nestedNode = toLiteral(model, (Double) value);
        }
        else if ((value instanceof Boolean) ||
                (value instanceof Number))
        {
            if (onlyNested)
            {
                return;
            }

            nestedNode = model.createTypedLiteral(value);
        }
        else if (value instanceof URI)
        {
            if (onlyNested)
            {
                return;
            }

            final URI uri = (URI) value;

            if (OSLC4JUtils.relativeURIsAreDisabled() && !uri.isAbsolute())
            {
                throw new OslcCoreRelativeURIException(resourceClass,
                        (method == null) ? "<none>" : method.getName(),
                        uri);
            }

            // URIs represent references to other resources identified by their IDs, so they need to
            // be managed as such
            nestedNode = model.createResource(value.toString());
        }
        else if (value instanceof Date)
        {
            if (onlyNested)
            {
                return;
            }

            final GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTime((Date) value);
            RDFDatatype dataType = null;

            // Check if it is a Date field
            if ( OSLC4JUtils.inferTypeFromShape() ) {
                // get the list of resource rdf type
                HashSet<String> rdfTypes = new HashSet<>();
                rdfTypes = getTypesFromResource(resource, rdfTypes);
                dataType = OSLC4JUtils.getDataTypeBasedOnResourceShapeType(rdfTypes, attribute);
            }

            if ( dataType != null && dataType instanceof XSDDateType) {
                XSDDateTime valuec = new XSDDateTime( calendar);
                valuec.narrowType(XSDDatatype.XSDdate);
                String valueDate = valuec.toString();
                if ( valueDate != null && valueDate.endsWith("Z")){
                    valueDate = valueDate.replaceAll("Z","");
                }
                nestedNode = model.createTypedLiteral(valueDate, XSDDatatype.XSDdate);
            } else {
                nestedNode = model.createTypedLiteral(calendar);
            }
        }
        else if (objectClass.getAnnotation(OslcResourceShape.class) != null)
        {
            final String namespace = TypeFactory.getNamespace(objectClass);
            final String name	   = TypeFactory.getName(objectClass);

            URI aboutURI = null;
            if (value instanceof IResource)
            {
                aboutURI = ((IResource) value).getAbout();
            }

            final Resource nestedResource;
            if (aboutURI != null)
            {
                if (OSLC4JUtils.relativeURIsAreDisabled() && !aboutURI.isAbsolute())
                {
                    throw new OslcCoreRelativeURIException(objectClass,
                            "getAbout",
                            aboutURI);
                }

                nestedResource = model.createResource(aboutURI.toString(),
                        model.createProperty(namespace,
                                name));
            }
            else
            {
                nestedResource = model.createResource(model.createProperty(namespace,
                        name));
            }

            buildResource(value,
                    objectClass,
                    model,
                    nestedResource,
                    nestedProperties);

            nestedNode = nestedResource;
        }
        else if (logger.isWarnEnabled())
        {
            // Warn that one of the properties could not be serialized because it does not have the
            // right annotations.
            String subjectClassName = resourceClass.getSimpleName();
            if ("".equals(subjectClassName))
            {
                subjectClassName = resourceClass.getName();
            }

            String objectClassName = objectClass.getSimpleName();
            if ("".equals(objectClassName))
            {
                objectClassName = objectClass.getName();
            }

            logger.warn("Could not serialize "
                    + objectClassName
                    + " because it does not have an OslcResourceShape annotation "
                    + "(class: "
                    + subjectClassName + ", method: " + method.getName() + ")");
        }

        if (nestedNode != null) {
            if (rdfNodeContainer != null) {
                if (reifiedResource != null)
                {
                    throw new IllegalStateException(
                            "Reified resource is not supported for rdf collection resources");
                }

                // Instead of adding a nested node to model, add it to a list
                rdfNodeContainer.add(nestedNode);
            }
            else
            {
                Statement statement = model.createStatement(resource, attribute, nestedNode);

                if (reifiedResource != null &&
                        nestedProperties != OSLC4JConstants.OSL4J_PROPERTY_SINGLETON)
                {
                    addReifiedStatements(model, statement, reifiedResource, nestedProperties);
                }

                // Finally, add the statement to the model.
                model.add(statement);
            }
        }
    }

    private static void addReifiedStatements(final Model			   model,
                                             final Statement		   statement,
                                             final IReifiedResource<?> reifiedResource,
                                             final Map<String, Object> nestedProperties)
            throws IllegalArgumentException,
            IllegalAccessException,
            InvocationTargetException,
            DatatypeConfigurationException,
            OslcCoreApplicationException
    {
        ReifiedStatement reifiedStatement = statement.createReifiedStatement();

        buildResource(reifiedResource,
                reifiedResource.getClass(),
                model,
                reifiedStatement,
                nestedProperties);

        //https://bugs.eclipse.org/bugs/show_bug.cgi?id=526188
        //If the resulting reifiedStatement only contain the 4 statements about its subject,
        // predicate object, & type, then there are no additional statements on the statement. Hence,
        // remove the newly created reifiedStatement.
        if (reifiedStatement.listProperties().toList().size() == 4) {
            reifiedStatement.removeProperties();
            logger.debug("An empty reified statement was stripped from the model");
        }
    }

    private static String getDefaultPropertyName(final Method method)
    {
        final String methodName	   = method.getName();
        final int	 startingIndex = methodName.startsWith(METHOD_NAME_START_GET)
                ? METHOD_NAME_START_GET_LENGTH
                : METHOD_NAME_START_IS_LENGTH;
        final int	 endingIndex   = startingIndex + 1;

        // We want the name to start with a lower-case letter
        final String lowercasedFirstCharacter = methodName.substring(startingIndex,
                endingIndex).toLowerCase(Locale.ENGLISH);

        if (methodName.length() == endingIndex)
        {
            return lowercasedFirstCharacter;
        }

        return lowercasedFirstCharacter + methodName.substring(endingIndex);
    }

    private static void recursivelyCollectNamespaceMappings(final Map<String, String>	  namespaceMappings,
                                                            final Class<?> resourceClass)
    {
        final OslcSchema oslcSchemaAnnotation = resourceClass.getPackage().getAnnotation(OslcSchema.class);

        if (oslcSchemaAnnotation != null)
        {
            final OslcNamespaceDefinition[] oslcNamespaceDefinitionAnnotations = oslcSchemaAnnotation.value();

            for (final OslcNamespaceDefinition oslcNamespaceDefinitionAnnotation :
                    oslcNamespaceDefinitionAnnotations)
            {
                final String prefix		  = oslcNamespaceDefinitionAnnotation.prefix();
                final String namespaceURI = oslcNamespaceDefinitionAnnotation.namespaceURI();

                namespaceMappings.put(prefix,
                        namespaceURI);
            }
            //Adding custom prefixes obtained from an implementation, if there is an implementation.
            Class<? extends IOslcCustomNamespaceProvider> customNamespaceProvider =
                    oslcSchemaAnnotation.customNamespaceProvider();
            if(!customNamespaceProvider.isInterface())
            {
                try {
                    IOslcCustomNamespaceProvider customNamespaceProviderImpl
                            = customNamespaceProvider.newInstance();
                    Map<String, String> customNamespacePrefixes = customNamespaceProviderImpl
                            .getCustomNamespacePrefixes();
                    if(null != customNamespacePrefixes)
                    {
                        namespaceMappings.putAll(customNamespacePrefixes);
                    }
                } catch (IllegalAccessException e) {
                    throw new RuntimeException("The custom namespace provider implementation: "+
                            customNamespaceProvider.getClass().getName() +
                            ", must have a public no args construtor", e);
                } catch (InstantiationException e) {
                    throw new RuntimeException("The custom namespace provider must not be a abstract," +
                            " nor interface class and must have a public no args constructor", e);
                }
            }
        }

        final Class<?> superClass = resourceClass.getSuperclass();

        if (superClass != null)
        {
            recursivelyCollectNamespaceMappings(namespaceMappings,
                    superClass);
        }

        final Class<?>[] interfaces = resourceClass.getInterfaces();

        if (interfaces != null)
        {
            for (final Class<?> interfac : interfaces)
            {
                recursivelyCollectNamespaceMappings(namespaceMappings,
                        interfac);
            }
        }
    }

    private static void ensureNamespacePrefix(final String				prefix,
                                              final String				namespace,
                                              final Map<String, String> namespaceMappings)
    {
        if (!namespaceMappings.containsValue(namespace))
        {
            if (!namespaceMappings.containsKey(prefix))
            {
                namespaceMappings.put(prefix,
                        namespace);
            }
            else
            {
                // There is already a namespace for this prefix.  We need to generate a new unique prefix.
                int index = 1;

                while (true)
                {
                    final String newPrefix = prefix + index;

                    if (!namespaceMappings.containsKey(newPrefix))
                    {
                        namespaceMappings.put(newPrefix, namespace);

                        return;
                    }

                    index++;
                }
            }
        }
    }

    private static String getVisitedResourceName(Resource resource)
    {
        String visitedResourceName;

        if (resource.getURI() != null)
        {
            visitedResourceName = resource.getURI();
        }
        else
        {
            visitedResourceName = resource.getId().toString();
        }

        return visitedResourceName;
    }

    private static Transformer createTransformer()
    {
        try
        {
            Transformer transformer = TransformerFactory.newInstance().newTransformer();

            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");

            return transformer;

        } catch (TransformerException e) {

            throw new RuntimeException(e);

        }
    }

}
