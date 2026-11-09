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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Optional;

import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.IResource;

/*
 * A pairing of getter/setter methods for one OSLC property on a Java class representing an OSLC Resource Shape.
 */
public class PropertyAccessor {

    private final String propertyDefinition;
    private final String propertyName;
    private final Method getter;
    private final Method setter;
    private final Optional<OslcValueType> valueType;
    private final Optional<OslcRdfCollectionType> collectionType;

    PropertyAccessor(String propertyDefinition, String propertyName, Method getter, Method setter) {
        this.propertyDefinition = propertyDefinition;
        this.propertyName = propertyName;
        this.getter = getter;
        this.setter = setter;
        this.valueType = Optional.ofNullable(MethodAnnotationCache.OSLC_VALUE_TYPE.getAnnotation(getter));
        this.collectionType = Optional.ofNullable(MethodAnnotationCache.OSLC_RDF_COLLECTION_TYPE.getAnnotation(getter));
    }

    public String getPropertyDefinition() {
        return propertyDefinition;
    }

    public QName getQName() {
        String namespace = propertyDefinition.substring(0, propertyDefinition.length() - propertyName.length());
        return new QName(namespace, propertyName);
    }

    public Optional<OslcValueType> getValueType() {
        return valueType;
    }

    public Optional<OslcRdfCollectionType> getCollectionType() {
        return collectionType;
    }

    public Method getGetter() {
        return getter;
    }

    Object getValue(IResource resource) {
        try {
            return getter.invoke(resource);
        } catch (IllegalArgumentException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e); // NOSONAR
        }
    }

    void setValue(IResource resource, Object value) {
        try {
            setter.invoke(resource, value);
        } catch (IllegalArgumentException e) {
            if ((value != null) && (setter.getParameterTypes().length == 1)) {
                String message = "Method '" + setter.getName() + "' called with " + value.getClass().getName() + " while expected " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        + setter.getParameterTypes()[0].getName();
                throw new IllegalArgumentException(message, e);
            } else {
                throw e;
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e); // NOSONAR
        }
    }

    public Method getSetter() {
        return setter;
    }

}
