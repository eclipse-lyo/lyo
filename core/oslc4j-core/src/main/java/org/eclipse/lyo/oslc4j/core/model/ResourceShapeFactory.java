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
package org.eclipse.lyo.oslc4j.core.model;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.eclipse.lyo.oslc4j.core.CoreHelper;
import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValues;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDefaultValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcHidden;
import org.eclipse.lyo.oslc4j.core.annotation.OslcMaxSize;
import org.eclipse.lyo.oslc4j.core.annotation.OslcMemberProperty;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreDuplicatePropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidOccursException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyTypeException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidRepresentationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidValueTypeException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingAnnotationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingSetMethodException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.ws.rs.core.UriBuilder;

public class ResourceShapeFactory {

    private final static Logger log = LoggerFactory.getLogger(ResourceShapeFactory.class);

    protected static final String METHOD_NAME_START_GET = "get";
	protected static final String METHOD_NAME_START_IS  = "is";
	protected static final String METHOD_NAME_START_SET = "set";

	protected static final int METHOD_NAME_START_GET_LENGTH = METHOD_NAME_START_GET.length();
	protected static final int METHOD_NAME_START_IS_LENGTH  = METHOD_NAME_START_IS.length();

	protected static final Map<Class<?>, ValueType> CLASS_TO_VALUE_TYPE = new HashMap<>();

	static {
		// Primitive types
		CLASS_TO_VALUE_TYPE.put(Boolean.TYPE, ValueType.Boolean);
		CLASS_TO_VALUE_TYPE.put(Byte.TYPE,	  ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Short.TYPE,	  ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Integer.TYPE, ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Long.TYPE,	  ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Float.TYPE,	  ValueType.Float);
		CLASS_TO_VALUE_TYPE.put(Double.TYPE,  ValueType.Double);

		// Object types
		CLASS_TO_VALUE_TYPE.put(Boolean.class,	  ValueType.Boolean);
		CLASS_TO_VALUE_TYPE.put(Byte.class,		  ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Short.class,	  ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Integer.class,	  ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Long.class,		  ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(BigInteger.class, ValueType.Integer);
		CLASS_TO_VALUE_TYPE.put(Float.class,	  ValueType.Float);
		CLASS_TO_VALUE_TYPE.put(Double.class,	  ValueType.Double);
		CLASS_TO_VALUE_TYPE.put(String.class,	  ValueType.String);
		CLASS_TO_VALUE_TYPE.put(Date.class,		  ValueType.DateTime);
		CLASS_TO_VALUE_TYPE.put(URI.class,		  ValueType.Resource);
	}

	protected ResourceShapeFactory() {
		super();
	}

	public static ResourceShape createResourceShape(final String baseURI,
													final String resourceShapesPath,
													final String resourceShapePath,
													final Class<?> resourceClass)
		   throws OslcCoreApplicationException, URISyntaxException {
		final HashSet<Class<?>> verifiedClasses = new HashSet<>();
		verifiedClasses.add(resourceClass);

		return createResourceShape(baseURI, resourceShapesPath, resourceShapePath, resourceClass, verifiedClasses);
	}

	private static ResourceShape createResourceShape(final String baseURI,
													final String resourceShapesPath,
													final String resourceShapePath,
													final Class<?> resourceClass,
													final Set<Class<?>> verifiedClasses)
		   throws OslcCoreApplicationException, URISyntaxException {
		final OslcResourceShape resourceShapeAnnotation = resourceClass.getAnnotation(OslcResourceShape.class);
		if (resourceShapeAnnotation == null) {
			throw new OslcCoreMissingAnnotationException(resourceClass, OslcResourceShape.class);
		}

		final URI about = UriBuilder.fromUri(baseURI).path(resourceShapesPath).path(resourceShapePath).build();
		final ResourceShape resourceShape = new ResourceShape(about);

        final OslcName nameAnnotation = resourceClass.getAnnotation(OslcName.class);
        if (nameAnnotation != null) {
            resourceShape.setName(nameAnnotation.value());
        } else {
            resourceShape.setName(resourceClass.getSimpleName());
        }

		final String title = resourceShapeAnnotation.title();
		if ((title != null) && (title.length() > 0)) {
			resourceShape.setTitle(title);
		}

	      final String description = resourceShapeAnnotation.description();
	        if ((description != null) && (description.length() > 0)) {
	            resourceShape.setDescription(description);
	        }

		for (final String describesItem : resourceShapeAnnotation.describes()) {
			resourceShape.addDescribeItem(new URI(describesItem));
		}

		final Set<String> propertyDefinitions = new HashSet<>();

		for (final Method method : resourceClass.getMethods()) {
			if (method.getParameterTypes().length == 0) {
				final String methodName = method.getName();
				final int methodNameLength = methodName.length();
				if (((methodName.startsWith(METHOD_NAME_START_GET)) && (methodNameLength > METHOD_NAME_START_GET_LENGTH)) ||
					((methodName.startsWith(METHOD_NAME_START_IS)) && (methodNameLength > METHOD_NAME_START_IS_LENGTH))) {
					final OslcPropertyDefinition propertyDefinitionAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcPropertyDefinition.class);
					if (propertyDefinitionAnnotation != null) {
						final String propertyDefinition = propertyDefinitionAnnotation.value();
						if (propertyDefinitions.contains(propertyDefinition)) {
							throw new OslcCoreDuplicatePropertyDefinitionException(resourceClass, propertyDefinitionAnnotation);
						}

						propertyDefinitions.add(propertyDefinition);

						final Property property = createProperty(baseURI, resourceClass, method, propertyDefinitionAnnotation, verifiedClasses);
						resourceShape.addProperty(property);

						validateSetMethodExists(resourceClass, method);
					}
				}
			}
		}

		return resourceShape;
	}

	@SuppressWarnings("rawtypes") // supress warning when casting Arrays.asList() to a Collection
	private static Property createProperty(final String baseURI, final Class<?> resourceClass, final Method method, final OslcPropertyDefinition propertyDefinitionAnnotation, final Set<Class<?>> verifiedClasses) throws OslcCoreApplicationException, URISyntaxException {
		final String name;
		final OslcName nameAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcName.class);
		if (nameAnnotation != null) {
			name = nameAnnotation.value();
		} else {
			name = getDefaultPropertyName(method);
		}

		final String propertyDefinition = propertyDefinitionAnnotation.value();

		if (!propertyDefinition.endsWith(name)) {
			throw new OslcCoreInvalidPropertyDefinitionException(resourceClass, method, propertyDefinitionAnnotation);
		}

		final Class<?> returnType = method.getReturnType();
		final Occurs occurs;
		final OslcOccurs occursAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcOccurs.class);
		if (occursAnnotation != null) {
			occurs = occursAnnotation.value();
			validateUserSpecifiedOccurs(resourceClass, method, occursAnnotation);
		} else {
			occurs = getDefaultOccurs(returnType);
		}

		Class<?> componentType = getComponentType(resourceClass, method, returnType);

		// Reified resources are a special case.
		if (IReifiedResource.class.isAssignableFrom(componentType))
		{
			final Type genericType = componentType.getGenericSuperclass();

			if (genericType instanceof ParameterizedType)
			{
				final ParameterizedType parameterizedType = (ParameterizedType) genericType;
				final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
				if (actualTypeArguments.length == 1)
				{
					final Type actualTypeArgument = actualTypeArguments[0];
                    if (actualTypeArgument instanceof Class) {
                        componentType = CoreHelper.getActualTypeArgument(actualTypeArgument);
                    } else if (actualTypeArgument instanceof TypeVariable) {
                        log.error("Resource shapes are not designed to be initialized with the help of GenericEntity " +
                            "(or other ways to capture a TypeVariable)");
                    }
                }
			}
		}

		final ValueType valueType;
		final OslcValueType valueTypeAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcValueType.class);
		if (valueTypeAnnotation != null) {
			valueType = valueTypeAnnotation.value();
		} else {
			valueType = getDefaultValueType(resourceClass, method, componentType);
		}

		final Property property = new Property(name, occurs, new URI(propertyDefinition), valueType);

		property.setTitle(property.getName());
		final OslcTitle titleAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcTitle.class);
		if (titleAnnotation != null) {
			property.setTitle(titleAnnotation.value());
		}

		final OslcDescription descriptionAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcDescription.class);
		if (descriptionAnnotation != null) {
			property.setDescription(descriptionAnnotation.value());
		}

		final OslcRange rangeAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcRange.class);
		if (rangeAnnotation != null) {
			for (final String range : rangeAnnotation.value()) {
				property.addRange(new URI(range));
			}
		}

        final Representation representation;
		final OslcRepresentation representationAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcRepresentation.class);
		if (representationAnnotation != null) {
			representation = representationAnnotation.value();
			property.setRepresentation(new URI(representation.toString()));
		} else {
			representation = getDefaultRepresentation(componentType);
			if (representation != null) {
				property.setRepresentation(new URI(representation.toString()));
			}
		}

		final OslcAllowedValue allowedValueAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcAllowedValue.class);

		if (allowedValueAnnotation != null) {
			property.setAllowedValuesCollection((Collection) Arrays.asList(allowedValueAnnotation.value()));
		}

		final OslcAllowedValues allowedValuesAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcAllowedValues.class);
		if (allowedValuesAnnotation != null) {
			property.setAllowedValuesRef(new URI(allowedValuesAnnotation.value()));
		}

		final OslcDefaultValue defaultValueAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcDefaultValue.class);
		if (defaultValueAnnotation != null) {
			property.setDefaultValue(defaultValueAnnotation.value());
		}

		final OslcHidden hiddenAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcHidden.class);
		if (hiddenAnnotation != null) {
			property.setHidden(hiddenAnnotation.value());
		}

		final OslcMemberProperty memberPropertyAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcMemberProperty.class);
		if (memberPropertyAnnotation != null) {
			property.setMemberProperty(memberPropertyAnnotation.value());
		}

		final OslcReadOnly readOnlyAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcReadOnly.class);
		if (readOnlyAnnotation != null) {
			property.setReadOnly(readOnlyAnnotation.value());
		}

		final OslcMaxSize maxSizeAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcMaxSize.class);
		if (maxSizeAnnotation != null) {
			property.setMaxSize(maxSizeAnnotation.value());
		}

		final OslcValueShape valueShapeAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, OslcValueShape.class);
		if (valueShapeAnnotation != null) {
			property.setValueShape(new URI(baseURI + "/" + valueShapeAnnotation.value()));
		}

        validateUserSpecifiedValueType(resourceClass, method, valueType, representation, componentType);
        validateUserSpecifiedRepresentation(resourceClass, method, representation, componentType);
		if ((ValueType.LocalResource.equals(valueType))
		    || (ValueType.Resource.equals(valueType) && (null != representation) && (Representation.Inline.equals(representation)))) {
			// If this is a nested class we potentially have not yet verified
			if (verifiedClasses.add(componentType)) {
				// Validate nested resource ignoring return value, but throwing any exceptions
				createResourceShape(baseURI, OslcConstants.PATH_RESOURCE_SHAPES, "unused", componentType, verifiedClasses);
			}
		}

		return property;
	}

	protected static String getDefaultPropertyName(final Method method) {
		final String methodName	   = method.getName();
		final int	 startingIndex = methodName.startsWith(METHOD_NAME_START_GET) ? METHOD_NAME_START_GET_LENGTH : METHOD_NAME_START_IS_LENGTH;
		final int	 endingIndex   = startingIndex + 1;

		// We want the name to start with a lower-case letter
		final String lowercasedFirstCharacter = methodName.substring(startingIndex, endingIndex).toLowerCase(Locale.ENGLISH);
		if (methodName.length() == endingIndex) {
			return lowercasedFirstCharacter;
		}

		return lowercasedFirstCharacter + methodName.substring(endingIndex);
	}

	private static ValueType getDefaultValueType(final Class<?> resourceClass, final Method method, final Class<?> componentType) throws OslcCoreApplicationException {
		final ValueType valueType = CLASS_TO_VALUE_TYPE.get(componentType);
		if (valueType == null) {
			throw new OslcCoreInvalidPropertyTypeException(resourceClass, method, componentType);
		}
		return valueType;
	}

	private static Representation getDefaultRepresentation(final Class<?> componentType) {
		if (componentType.equals(URI.class)) {
			return Representation.Reference;
		}
		return null;
	}

	private static Occurs getDefaultOccurs(final Class<?> type) {
		if ((type.isArray()) ||
			(Collection.class.isAssignableFrom(type))) {
			return Occurs.ZeroOrMany;
		}
		return Occurs.ZeroOrOne;
	}

	protected static Class<?> getComponentType(final Class<?> resourceClass, final Method method, final Class<?> type) throws OslcCoreInvalidPropertyTypeException {
		if (type.isArray()) {
			return type.getComponentType();
		} else if (Collection.class.isAssignableFrom(type)) {
			final Type genericReturnType = method.getGenericReturnType();
			if (genericReturnType instanceof ParameterizedType) {
				final ParameterizedType parameterizedType = (ParameterizedType) genericReturnType;
				final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
				if (actualTypeArguments.length == 1) {
					final Type actualTypeArgument = actualTypeArguments[0];
					if (actualTypeArgument instanceof Class) {
						return (Class<?>) actualTypeArgument;
					}
				}
			}
			throw new OslcCoreInvalidPropertyTypeException(resourceClass, method, type);
		} else {
			return type;
		}
	}

	protected static void validateSetMethodExists(final Class<?> resourceClass, final Method getMethod) throws OslcCoreMissingSetMethodException {
		final String getMethodName = getMethod.getName();

		final String setMethodName;
		if (getMethodName.startsWith(METHOD_NAME_START_GET)) {
			setMethodName = METHOD_NAME_START_SET + getMethodName.substring(METHOD_NAME_START_GET_LENGTH);
		} else {
			setMethodName = METHOD_NAME_START_SET + getMethodName.substring(METHOD_NAME_START_IS_LENGTH);
		}

        final Class<?> returnType = getMethod.getReturnType();
        if(!isCollectionType(returnType)) {
            try {
                resourceClass.getMethod(setMethodName, returnType);
            } catch (final NoSuchMethodException exception) {
                throw new OslcCoreMissingSetMethodException(resourceClass, getMethod, exception);
            }
        } else {
            final List<Method> methods = Arrays.stream(resourceClass.getMethods())
                                               .filter(m -> m.getName().equals(setMethodName))
                                               .collect(Collectors.toList());
            if (methods.size() == 0) {
                throw new OslcCoreMissingSetMethodException(resourceClass, getMethod, null);
            }

            if (methods.size() > 1) {
                throw new IllegalArgumentException(
                        "Multiple setters on the same RDF property are not yet supported");
            }

            final Method method = methods.get(0);

            final Type[] parameterTypes = method.getGenericParameterTypes();
            if (parameterTypes == null || parameterTypes.length == 0 || parameterTypes.length > 1) {
                throw new IllegalArgumentException(
                        String.format("Method '%s' shall have exactly one argument",
                                      method.getName()
                        ));
            }

            final ParameterizedType setterArgumentType = (ParameterizedType) parameterTypes[0];
            final ParameterizedType genericReturnType = (ParameterizedType) getMethod.getGenericReturnType();
            if (!genericReturnType.getActualTypeArguments()[0].equals(setterArgumentType.getActualTypeArguments()[0])) {
                // if a getter returns HashSet<T>, we want the same t in the setter Set<T>
                throw new OslcCoreMissingSetMethodException(resourceClass, getMethod, null);
            }
        }
	}

    static boolean isCollectionType(final Class<?> returnType) {
	    return Collection.class.isAssignableFrom(returnType);
    }

    private static void validateUserSpecifiedOccurs(final Class<?> resourceClass, final Method method, final OslcOccurs occursAnnotation) throws OslcCoreInvalidOccursException {
		final Class<?> returnType = method.getReturnType();
		final Occurs   occurs	  = occursAnnotation.value();

		if ((returnType.isArray()) ||
			(Collection.class.isAssignableFrom(returnType))) {
			if ((!Occurs.ZeroOrMany.equals(occurs)) &&
				(!Occurs.OneOrMany.equals(occurs))) {
				throw new OslcCoreInvalidOccursException(resourceClass, method, occursAnnotation);
			}
		} else {
			if ((!Occurs.ZeroOrOne.equals(occurs)) &&
				(!Occurs.ExactlyOne.equals(occurs))) {
				 throw new OslcCoreInvalidOccursException(resourceClass, method, occursAnnotation);
			}
		}
	}

	protected static void validateUserSpecifiedValueType(final Class<?> resourceClass, final Method method, final ValueType userSpecifiedValueType, final Representation userSpecifiedRepresentation, final Class<?> componentType) throws OslcCoreInvalidValueTypeException {
		final ValueType calculatedValueType = CLASS_TO_VALUE_TYPE.get(componentType);
		// ValueType is valid if ...
		// user-specified value type matches calculated value type
		// or
		// user-specified value type is local resource (we will validate the local resource later)
        // or
        // user-specified value type is non-local resource, and Representation is Inline (we will validate later)
		// or
		// user-specified value type is xml literal and calculated value type is string
		// or
		// user-specified value type is decimal and calculated value type is numeric
		if ((userSpecifiedValueType.equals(calculatedValueType))
			||
			(ValueType.LocalResource.equals(userSpecifiedValueType))
			||
            (ValueType.Resource.equals(userSpecifiedValueType) && (null != userSpecifiedRepresentation) && (Representation.Inline.equals(userSpecifiedRepresentation)))
            ||
			((ValueType.XMLLiteral.equals(userSpecifiedValueType))
			 &&
			 (ValueType.String.equals(calculatedValueType))
			)
			||
			((ValueType.Decimal.equals(userSpecifiedValueType))
			 &&
			 ((ValueType.Double.equals(calculatedValueType))
			  ||
			  (ValueType.Float.equals(calculatedValueType))
			  ||
			  (ValueType.Integer.equals(calculatedValueType))
			 )
			)
		   ) {
			// We have a valid user-specified value type for our Java type
			return;
		}

		throw new OslcCoreInvalidValueTypeException(resourceClass, method, userSpecifiedValueType);
	}

	private static void validateUserSpecifiedRepresentation(final Class<?> resourceClass, final Method method, final Representation userSpecifiedRepresentation, final Class<?> componentType) throws OslcCoreInvalidRepresentationException {
	    // Throw an Exception if ...
	    // User-specified representation is reference and component is not URI
		// or
		// user-specified representation is inline and component is a standard class

	    if (null == userSpecifiedRepresentation) {
	        return;
	    }
		if (((Representation.Reference.equals(userSpecifiedRepresentation))
			 &&
			 (!URI.class.equals(componentType))
			)
			||
			((Representation.Inline.equals(userSpecifiedRepresentation))
			 &&
			 (CLASS_TO_VALUE_TYPE.containsKey(componentType))
			)
		   ) {
			throw new OslcCoreInvalidRepresentationException(resourceClass, method, userSpecifiedRepresentation);
		}
	}
}
