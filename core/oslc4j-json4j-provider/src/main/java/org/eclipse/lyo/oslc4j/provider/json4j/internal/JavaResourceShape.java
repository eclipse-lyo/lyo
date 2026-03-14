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

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingSetMethodException;

/*
 * Provides facilities related to a Java class representing an OSLC Resource Shape.
 * Gives access to property accessors, a property accessor being the pairing of getter/setter methods for one OSLC property.
 * Enables getting and setting values, either through a property accessor or extended properties.
 */
public class JavaResourceShape {
    private static final String METHOD_NAME_START_GET = "get"; //$NON-NLS-1$
    private static final String METHOD_NAME_START_IS = "is"; //$NON-NLS-1$
    private static final String METHOD_NAME_START_SET = "set"; //$NON-NLS-1$

    private static final Map<Class<?>, JavaResourceShape> SHAPES = new HashMap<>();

    public static synchronized JavaResourceShape valueOf(Class<?> resourceClass) throws OslcCoreApplicationException {
        JavaResourceShape shape = SHAPES.get(resourceClass);
        if (shape == null) {
            shape = JavaResourceShapeFactory.create(resourceClass);
            SHAPES.put(resourceClass, shape);
        }
        return shape;
    }

    private final Map<String, PropertyAccessor> accessors;

    private JavaResourceShape() {
        this.accessors = new HashMap<>();
    }

    public Collection<PropertyAccessor> getAccessors() {
        return Collections.unmodifiableCollection(this.accessors.values());
    }

    public Optional<PropertyAccessor> getAccessor(String propertyDefinition) {
        return Optional.ofNullable(this.accessors.get(propertyDefinition));
    }

    private static class JavaResourceShapeFactory {

        private static JavaResourceShape create(Class<?> resourceClass) throws OslcCoreApplicationException {
            JavaResourceShape shape = new JavaResourceShape();

            // look for methods without any parameter
            for (final Method method : resourceClass.getMethods()) {
                if (method.getParameterTypes().length == 0) {
                    final String getMethodName = method.getName();

                    // getSomething()?
                    if (getMethodName.startsWith(METHOD_NAME_START_GET) && (getMethodName.length() > METHOD_NAME_START_GET.length())) {
                        parse(resourceClass, shape, method, METHOD_NAME_START_GET);
                    }

                    // isSomething()?
                    else if (getMethodName.startsWith(METHOD_NAME_START_IS) && (getMethodName.length() > METHOD_NAME_START_IS.length())) {
                        parse(resourceClass, shape, method, METHOD_NAME_START_IS);
                    }
                }
            }

            return shape;
        }

        private static void parse(Class<?> resourceClass, JavaResourceShape shape, Method getter, String prefix) throws OslcCoreApplicationException {

            // retain the getter only if there's an OSLC annotation
            OslcPropertyDefinition oslcPropertyDefinitionAnnotation = MethodAnnotationCache.OSLC_PROPERTY_DEFINITION.getAnnotation(getter);
            if (oslcPropertyDefinitionAnnotation != null) {

                // We need to find the set companion setMethod
                String setMethodName = METHOD_NAME_START_SET + getter.getName().substring(prefix.length());
                Class<?> getMethodReturnType = getter.getReturnType();
                try {
                    final Method setter = resourceClass.getMethod(setMethodName, getMethodReturnType);
                    addAccessor(resourceClass, shape, oslcPropertyDefinitionAnnotation, getter, prefix, setter);
                } catch (final NoSuchMethodException exception) {
                    throw new OslcCoreMissingSetMethodException(resourceClass, getter, exception);
                }
            }
        }

        private static void addAccessor(Class<?> resourceClass, JavaResourceShape shape, OslcPropertyDefinition propertyDefinitionAnnotation,
                Method getter, String prefix, Method setter) throws OslcCoreApplicationException {

            String propertyDefinition = propertyDefinitionAnnotation.value();

            String propertyName = getPropertyName(getter, prefix);
            if (!propertyDefinition.endsWith(propertyName)) {
                throw new OslcCoreInvalidPropertyDefinitionException(resourceClass, getter, propertyDefinitionAnnotation);
            }

            PropertyAccessor accessor = new PropertyAccessor(propertyDefinition, propertyName, getter, setter);
            shape.accessors.put(propertyDefinition, accessor);
        }

        private static String getPropertyName(Method getter, String prefix) {
            final OslcName nameAnnotation = MethodAnnotationCache.OSLC_NAME.getAnnotation(getter);
            if (nameAnnotation != null) {
                return nameAnnotation.value();
            } else {
                return getDefaultPropertyName(getter, prefix);
            }
        }

        private static String getDefaultPropertyName(Method method, String prefix) {
            String methodName = method.getName();
            int startingIndex = prefix.length();
            int endingIndex = startingIndex + 1;

            // We want the name to start with a lower-case letter
            final String lowercasedFirstCharacter = methodName.substring(startingIndex, endingIndex).toLowerCase(Locale.ENGLISH);

            if (methodName.length() == endingIndex) {
                return lowercasedFirstCharacter;
            }

            return lowercasedFirstCharacter + methodName.substring(endingIndex);
        }
    }
}
