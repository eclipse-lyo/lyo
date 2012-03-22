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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.AbstractSequentialList;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Deque;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Queue;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespaceDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingSetMethodException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMisusedOccursException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreRelativeURIException;
import org.eclipse.lyo.oslc4j.core.model.IExtendedResource;
import org.eclipse.lyo.oslc4j.core.model.IReifiedResource;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.core.model.InheritedMethodAnnotationHelper;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.TypeFactory;
import org.eclipse.lyo.oslc4j.core.model.AnyResource;

import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.RSIterator;
import com.hp.hpl.jena.rdf.model.ReifiedStatement;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

public final class JenaModelHelper
{
    private static final String PROPERTY_TOTAL_COUNT = "totalCount";

    private static final String RDF_TYPE_URI = OslcConstants.RDF_NAMESPACE + "type";

    private static final String METHOD_NAME_START_GET = "get";
    private static final String METHOD_NAME_START_IS  = "is";
    private static final String METHOD_NAME_START_SET = "set";

    private static final int METHOD_NAME_START_GET_LENGTH = METHOD_NAME_START_GET.length();
    private static final int METHOD_NAME_START_IS_LENGTH  = METHOD_NAME_START_IS.length();
    
    private static final String GENERATED_PREFIX_START = "j.";

    private static final Logger logger = Logger.getLogger(JenaModelHelper.class.getName());

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
                               objects);
    }

    static Model createJenaModel(final String   descriptionAbout,
                                 final String   responseInfoAbout,
                                 final Object[] objects)
           throws DatatypeConfigurationException,
                  IllegalAccessException,
                  IllegalArgumentException,
                  InvocationTargetException,
                  OslcCoreApplicationException
    {
        final Model               model             = ModelFactory.createDefaultModel();
        final Map<String, String> namespaceMappings = new HashMap<String, String>();

        Resource descriptionResource = null;

        if (descriptionAbout != null)
        {
            descriptionResource = model.createResource(descriptionAbout);

            if (responseInfoAbout != null)
            {
                final Resource responseInfoResource = model.createResource(responseInfoAbout,
                                                                           model.createProperty(OslcConstants.TYPE_RESPONSE_INFO));

                responseInfoResource.addProperty(model.createProperty(OslcConstants.OSLC_CORE_NAMESPACE,
                                                                      PROPERTY_TOTAL_COUNT),
                                                 String.valueOf(objects.length));
            }
        }

        for (final Object object : objects)
        {
            handleSingleResource(descriptionResource,
                                 object,
                                 model,
                                 namespaceMappings);
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

        return model;
    }

    private static void handleSingleResource(final Resource            descriptionResource,
                                             final Object              object,
                                             final Model               model,
                                             final Map<String, String> namespaceMappings)
            throws DatatypeConfigurationException,
                   IllegalAccessException,
                   IllegalArgumentException,
                   InvocationTargetException,
                   OslcCoreApplicationException
    {
        final Class<? extends Object> objectClass = object.getClass();

        if (objectClass.getAnnotation(OslcResourceShape.class) != null)
        {
            // Collect the namespace prefix -> namespace mappings
            recursivelyCollectNamespaceMappings(namespaceMappings,
                                                objectClass);

            final String namespace = TypeFactory.getNamespace(objectClass);
            final String name      = TypeFactory.getName(objectClass);

            URI aboutURI = null;
            if (object instanceof IResource)
            {
                aboutURI = ((IResource) object).getAbout();
            }

            final Resource mainResource;
            if (aboutURI != null)
            {
                if (!aboutURI.isAbsolute())
                {
                    throw new OslcCoreRelativeURIException(objectClass,
                                                           "getAbout",
                                                           aboutURI);
                }

                mainResource = model.createResource(aboutURI.toString(),
                                                    model.createProperty(namespace,
                                                                         name));
            }
            else
            {
                mainResource = model.createResource(model.createProperty(namespace,
                                                                         name));
            }

            buildResource(object,
                          objectClass,
                          model,
                          mainResource);

            if (descriptionResource != null)
            {
                descriptionResource.addProperty(RDFS.member,
                                                mainResource);
            }
        }
    }

    public static Object[] fromJenaModel(final Model    model,
                                         final Class<?> beanClass)
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
        final List<Object> results = new ArrayList<Object>();

        if (beanClass.getAnnotation(OslcResourceShape.class) != null)
        {
            final String qualifiedName = TypeFactory.getQualifiedName(beanClass);

            final ResIterator listSubjects = model.listSubjectsWithProperty(RDF.type,
                                                                            model.getResource(qualifiedName));

            if (listSubjects.hasNext())
            {
                final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods = new HashMap<Class<?>, Map<String, Method>>();

                while (listSubjects.hasNext())
                {
                    final Resource resource    = listSubjects.next();
                    final Object   newInstance = beanClass.newInstance();

                    fromResource(classPropertyDefinitionsToSetMethods,
                                 beanClass,
                                 newInstance,
                                 resource);

                    results.add(newInstance);
                }
            }
        }

        return results.toArray((Object[]) Array.newInstance(beanClass,
                                                            results.size()));
    }

    @SuppressWarnings("unchecked")
	private static void fromResource(final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods,
                                     final Class<?>                           beanClass,
                                     final Object                             bean,
                                     final Resource                           resource)
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

        if (bean instanceof IResource)
        {
            final String aboutURIString = resource.getURI();
            if (aboutURIString != null)
            {
                final URI aboutURI = new URI(aboutURIString);

                if (!aboutURI.isAbsolute())
                {
                    throw new OslcCoreRelativeURIException(beanClass,
                                                           "setAbout",
                                                           aboutURI);
                }

                ((IResource) bean).setAbout(aboutURI);
            }
        }

        // Collect values for array properties. We do this since values for
        // arrays are not required to be contiguous.
        final Map<String, List<Object>> propertyDefinitionsToArrayValues = new HashMap<String, List<Object>>();

        // Ensure a single-value property is not set more than once
        final Set<Method> singleValueMethodsUsed = new HashSet<Method>();

        final StmtIterator listProperties = resource.listProperties();

        final Map<QName, Object> extendedProperties;
        if (bean instanceof IExtendedResource)
        {
        	final IExtendedResource extendedResource = (IExtendedResource) bean;
        	extendedProperties = new HashMap<QName, Object>();
        	extendedResource.setExtendedProperties(extendedProperties);
        }
        else
        {
        	extendedProperties = null;
        }
        
        while (listProperties.hasNext())
        {
            final Statement statement = listProperties.next();
            final Property  predicate = statement.getPredicate();
            final RDFNode   object    = statement.getObject();

            final String uri       = predicate.getURI();
            final Method setMethod = setMethodMap.get(uri);
            if (setMethod == null)
            {
                if (RDF_TYPE_URI.equals(uri))
                {
                    // Ignore missing propertyDefinition for rdf:type.
                }
                else
                {
                	if (extendedProperties == null)
                	{
                        logger.warning("Set method not found for object type:  " +
                                       beanClass.getName() +
                                       ", uri:  " +
                                       uri);
                	}
                	else
                	{
                		String prefix = resource.getModel().getNsURIPrefix(predicate.getNameSpace());
                		if (prefix == null)
                		{
                			prefix = generatePrefix(resource.getModel(), predicate.getNameSpace());
                		}
                		final QName key = new QName(predicate.getNameSpace(), predicate.getLocalName(), prefix);
                		final Object value = handleExtendedPropertyValue(beanClass, object);
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
                				collection = new ArrayList<Object>();
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
                
                Object parameter = null;
                if (object.isLiteral())
                {
                    final Literal literal    = object.asLiteral();
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
                            throw new IllegalArgumentException("'" + stringValue + "' has wrong format for Boolean.");
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
                        parameter = Float.valueOf(stringValue);
                    }
                    else if ((Double.class == setMethodComponentParameterClass) ||
                             (Double.TYPE == setMethodComponentParameterClass))
                    {
                        parameter = Double.valueOf(stringValue);
                    }
                    else if (Date.class == setMethodComponentParameterClass)
                    {
                        parameter = DatatypeFactory.newInstance().newXMLGregorianCalendar(stringValue).toGregorianCalendar().getTime();
                    }
                }
                else if (object.isResource())
                {
                    final Resource nestedResource = object.asResource();

                    if (URI.class == setMethodComponentParameterClass)
                    {
                        final URI nestedResourceURI = new URI(nestedResource.getURI());

                        if (!nestedResourceURI.isAbsolute())
                        {
                            throw new OslcCoreRelativeURIException(beanClass,
                                                                   setMethod.getName(),
                                                                   nestedResourceURI);
                        }

                        parameter = nestedResourceURI;
                    }
                    else
                    {
                        final Object nestedBean = setMethodComponentParameterClass.newInstance();

                        fromResource(classPropertyDefinitionsToSetMethods,
                                     setMethodComponentParameterClass,
                                     nestedBean,
                                     nestedResource);

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
					        if (parameterTypes.length == 1 && parameterTypes[0].isAssignableFrom(setMethodComponentParameterClass))
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
	                                reifiedStatement);
					    }
					    
                		parameter = reifiedResource;
                	}
                	
                    if (multiple)
                    {
                        List<Object> values = propertyDefinitionsToArrayValues.get(uri);
                        if (values == null)
                        {
                            values = new ArrayList<Object>();

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
                                         new Object[] {parameter});

                        singleValueMethodsUsed.add(setMethod);
                    }
                }
            }
        }

        // Now, handle array and collection values since all are collected.
        for (final Map.Entry<String, List<Object>> propertyDefinitionToArrayValues : propertyDefinitionsToArrayValues.entrySet())
        {
            final String       uri            = propertyDefinitionToArrayValues.getKey();
            final List<Object> values         = propertyDefinitionToArrayValues.getValue();
            final Method       setMethod      = setMethodMap.get(uri);
            final Class<?>     parameterClass = setMethod.getParameterTypes()[0];

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
                                 new Object[] {array});
            }
            // Else - we are dealing with a collection or a subclass of collection
            else
            {
                final Collection<Object> collection;

                // Handle the Collection, List, Deque, Queue interfaces.
                // Handle the AbstractCollection, AbstractList, AbstractSequentialList classes
                if ((Collection.class             == parameterClass) ||
                    (List.class                   == parameterClass) ||
                    (Deque.class                  == parameterClass) ||
                    (Queue.class                  == parameterClass) ||
                    (AbstractCollection.class     == parameterClass) ||
                    (AbstractList.class           == parameterClass) ||
                    (AbstractSequentialList.class == parameterClass))
                {
                    collection = new LinkedList<Object>();
                }
                // Handle the Set interface
                // Handle the AbstractSet class
                else if ((Set.class          == parameterClass) ||
                         (AbstractSet.class  == parameterClass))
                {
                    collection = new HashSet<Object>();
                }
                // Handle the SortedSet and NavigableSet interfaces
                else if ((SortedSet.class    == parameterClass) ||
                         (NavigableSet.class == parameterClass))
                {
                    collection = new TreeSet<Object>();
                }
                // Not handled above.  Let's try newInstance with possible failure.
                else
                {
                    final Collection<Object> tempCollection = ((Collection<Object>) parameterClass.newInstance());
                    collection = tempCollection;
                }

                collection.addAll(values);

                setMethod.invoke(bean,
                                 new Object[] {collection});
            }
        }
    }

	/**
	 * Generates a prefix for unrecognized namespaces when reading in unknown
	 * properties and content.
	 * 
	 * @param model
	 *            the model
	 * @param namespace
	 *            the unrecognized namespace URI that needs a prefix
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
												      final RDFNode object)
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
			final Object literalValue = object.asLiteral().getValue();
			if (literalValue instanceof XSDDateTime)
			{
				final XSDDateTime xsdDateTime = (XSDDateTime) literalValue;
				return xsdDateTime.asCalendar().getTime();
			}
			
			return literalValue;
		}
		
		final Resource nestedResource = object.asResource();
		
		// Is this an inline resource?
		if (nestedResource.getURI() == null || nestedResource.listProperties().hasNext())
		{
			final AnyResource any = new AnyResource();
            final Map<Class<?>, Map<String, Method>> classPropertyDefinitionsToSetMethods = new HashMap<Class<?>, Map<String, Method>>();
            fromResource(classPropertyDefinitionsToSetMethods,
                         AnyResource.class,
                         any,
                         nestedResource);

            // Preserve rdf:type information.
            final StmtIterator typeIter = nestedResource.listProperties(RDF.type);
            while (typeIter.hasNext())
            {
            	final Statement typeStatement = typeIter.next();
            	any.addType(new URI(typeStatement.getResource().getURI()));
            }
            
            return any;
		}

		// It's a resource reference.
		final URI nestedResourceURI = new URI(nestedResource.getURI());
		if (!nestedResourceURI.isAbsolute())
		{
			throw new OslcCoreRelativeURIException(beanClass, "<none>",
					nestedResourceURI);
		}

		return nestedResourceURI;
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

    private static void buildResource(final Object   object,
                                      final Class<?> resourceClass,
                                      final Model    model,
                                      final Resource mainResource)
            throws DatatypeConfigurationException,
                   IllegalAccessException,
                   IllegalArgumentException,
                   InvocationTargetException,
                   OslcCoreApplicationException
    {
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
                    final OslcPropertyDefinition oslcPropertyDefinitionAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method,
                                                                                                                                  OslcPropertyDefinition.class);

                    if (oslcPropertyDefinitionAnnotation != null)
                    {
                        final Object value = method.invoke(object);

                        if (value != null)
                        {
                            buildAttributeResource(resourceClass,
                                                   method,
                                                   oslcPropertyDefinitionAnnotation,
                                                   model,
                                                   mainResource,
                                                   value);
                        }
                    }
                }
            }
        }
        
        // Handle any extended properties.
        if (object instanceof IExtendedResource)
        {
        	final IExtendedResource extendedResource = (IExtendedResource) object;
        	handleExtendedProperties(resourceClass,
				                     model,
				                     mainResource,
				                     extendedResource);
        }
    }

	protected static void handleExtendedProperties(final Class<?> resourceClass,
					                               final Model model,
					                               final Resource mainResource,
					                               final IExtendedResource extendedResource)
		throws DatatypeConfigurationException,
			   IllegalAccessException,
			   InvocationTargetException,
			   OslcCoreApplicationException
	{
		for (Map.Entry<QName, ?> extendedProperty : extendedResource.getExtendedProperties().entrySet())
		{
			final QName qName = extendedProperty.getKey();
			final Property property = model.createProperty(qName.getNamespaceURI() + qName.getLocalPart());
			final Object value = extendedProperty.getValue();
			if (value instanceof Collection)
			{
				final Collection<?> collection = (Collection<?>) value;
				for (Object next : collection)
				{
					handleExtendedValue(resourceClass, next, model, mainResource, property);
				}
			}
			else
			{
				handleExtendedValue(resourceClass, value, model, mainResource, property);
			}
		}
	}
    
    private static void handleExtendedValue(final Class<?> objectClass,
    									    final Object value,
    									    final Model model,
    									    final Resource resource,
    									    final Property property)
    	throws DatatypeConfigurationException,
    		   IllegalAccessException,
    		   IllegalArgumentException,
    		   InvocationTargetException,
    		   OslcCoreApplicationException
    {
    	if (value instanceof AnyResource)
    	{
    		final AnyResource any = (AnyResource) value;
    		final Resource nestedResource;
    		final URI aboutURI = any.getAbout();
    		if (aboutURI != null)
    		{
    			if (!aboutURI.isAbsolute())
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
    			nestedResource.addProperty(RDF.type, model.createResource(type.toString()));
    		}

    		handleExtendedProperties(AnyResource.class, model, nestedResource, any);
    		resource.addProperty(property, nestedResource);
    	}
    	else if (value.getClass().getAnnotation(OslcResourceShape.class) != null || value instanceof URI)
    	{
			handleLocalResource(objectClass, null, value, model, resource, property);
    	}
    	else if (value instanceof Date)
    	{
    		final Calendar cal = Calendar.getInstance();
    		cal.setTime((Date) value);
    		resource.addProperty(property, model.createTypedLiteral(cal));
    	}
    	else
    	{
    		resource.addProperty(property, model.createTypedLiteral(value));
    	}
    }
    
    private static void buildAttributeResource(final Class<?>               resourceClass,
                                               final Method                 method,
                                               final OslcPropertyDefinition propertyDefinitionAnnotation,
                                               final Model                  model,
                                               final Resource               resource,
                                               final Object                 value)
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

        final Property attribute = model.createProperty(propertyDefinition);

        final Class<?> returnType = method.getReturnType();

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
                                    object,
                                    model,
                                    resource,
                                    attribute);
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
                                    object,
                                    model,
                                    resource,
                                    attribute);
            }
        }
        else
        {
            handleLocalResource(resourceClass,
                                method,
                                value,
                                model,
                                resource,
                                attribute);
        }
    }

    private static void handleLocalResource(final Class<?> resourceClass,
                                            final Method   method,
                                            final Object   object,
                                            final Model    model,
                                            final Resource resource,
                                            final Property attribute)
        throws DatatypeConfigurationException,
               IllegalAccessException,
               IllegalArgumentException,
               InvocationTargetException,
               OslcCoreApplicationException
    {
        final Class<? extends Object> objectClass = object.getClass();

        Statement statement = null;
        final IReifiedResource<?> reifiedResource = (object instanceof IReifiedResource) ? (IReifiedResource<?>) object : null;
        final Object value = (reifiedResource == null) ? object : reifiedResource.getValue();
        
        if ((value instanceof String)  ||
            (value instanceof Boolean) ||
            (value instanceof Number))
        {
            statement = model.createStatement(resource, attribute, value.toString());
        }
        else if (value instanceof URI)
        {
            final URI uri = (URI) value;

            if (!uri.isAbsolute())
            {
                throw new OslcCoreRelativeURIException(resourceClass,
                                                       (method == null) ? "<none>" : method.getName(),
                                                       uri);
            }


            // URIs represent references to other resources identified by their IDs, so they need to be managed as such
            statement = model.createStatement(resource, attribute, model.createResource(value.toString()));
        }
        else if (value instanceof Date)
        {
            final GregorianCalendar calendar = new GregorianCalendar();
            calendar.setTime((Date) value);

            final String string = DatatypeFactory.newInstance().newXMLGregorianCalendar(calendar).toString();

            statement = model.createStatement(resource, attribute, string);
        }
        else if (objectClass.getAnnotation(OslcResourceShape.class) != null)
        {
            final String namespace = TypeFactory.getNamespace(objectClass);
            final String name      = TypeFactory.getName(objectClass);

            URI aboutURI = null;
            if (value instanceof IResource)
            {
                aboutURI = ((IResource) value).getAbout();
            }

            final Resource nestedResource;
            if (aboutURI != null)
            {
                if (!aboutURI.isAbsolute())
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
                          nestedResource);

            statement = model.createStatement(resource, attribute, nestedResource);
        }

        if (statement != null)
        {
        	if (reifiedResource != null)
        	{
        		addReifiedStatements(model, statement, reifiedResource);
        	}

        	// Finally, add the statement to the model.
        	model.add(statement);
        }
    }

	private static void addReifiedStatements(final Model model,
			                                 final Statement statement,
			                                 final IReifiedResource<?> reifiedResource)
			throws IllegalArgumentException,
			       IllegalAccessException,
			       InvocationTargetException,
			       DatatypeConfigurationException,
			       OslcCoreApplicationException
    {
		ReifiedStatement reifiedStatement = statement.createReifiedStatement();
		buildResource(reifiedResource, reifiedResource.getClass(), model,
				reifiedStatement);
	}

    private static String getDefaultPropertyName(final Method method)
    {
        final String methodName    = method.getName();
        final int    startingIndex = methodName.startsWith(METHOD_NAME_START_GET) ? METHOD_NAME_START_GET_LENGTH : METHOD_NAME_START_IS_LENGTH;
        final int    endingIndex   = startingIndex + 1;

        // We want the name to start with a lower-case letter
        final String lowercasedFirstCharacter = methodName.substring(startingIndex,
                                                                     endingIndex).toLowerCase();

        if (methodName.length() == endingIndex)
        {
            return lowercasedFirstCharacter;
        }

        return lowercasedFirstCharacter +
               methodName.substring(endingIndex);
    }

    private static void recursivelyCollectNamespaceMappings(final Map<String, String>     namespaceMappings,
                                                            final Class<? extends Object> resourceClass)
    {
        final OslcSchema oslcSchemaAnnotation = resourceClass.getPackage().getAnnotation(OslcSchema.class);

        if (oslcSchemaAnnotation != null)
        {
            final OslcNamespaceDefinition[] oslcNamespaceDefinitionAnnotations = oslcSchemaAnnotation.value();

            for (final OslcNamespaceDefinition oslcNamespaceDefinitionAnnotation : oslcNamespaceDefinitionAnnotations)
            {
                final String prefix       = oslcNamespaceDefinitionAnnotation.prefix();
                final String namespaceURI = oslcNamespaceDefinitionAnnotation.namespaceURI();

                namespaceMappings.put(prefix,
                                      namespaceURI);
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

    private static void ensureNamespacePrefix(final String              prefix,
                                              final String              namespace,
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
                    final String newPrefix = prefix +
                                             index;

                    if (!namespaceMappings.containsKey(newPrefix))
                    {
                        namespaceMappings.put(newPrefix,
                                              namespace);

                        return;
                    }

                    index++;
                }
            }
        }
    }
}