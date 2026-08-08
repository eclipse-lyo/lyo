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

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.InheritedMethodAnnotationHelper;

/*
 * Use a cache to avoid searching annotations every time a method is accessed when serializing a resource.
 * https://sodius.atlassian.net/browse/SECOLLAB-596
 */
class MethodAnnotationCache<A extends Annotation> {

    static final MethodAnnotationCache<OslcName> OSLC_NAME = new MethodAnnotationCache<>(OslcName.class);
    static final MethodAnnotationCache<OslcValueType> OSLC_VALUE_TYPE = new MethodAnnotationCache<>(OslcValueType.class);
    static final MethodAnnotationCache<OslcRdfCollectionType> OSLC_RDF_COLLECTION_TYPE = new MethodAnnotationCache<>(OslcRdfCollectionType.class);
    static final MethodAnnotationCache<OslcPropertyDefinition> OSLC_PROPERTY_DEFINITION = new MethodAnnotationCache<>(OslcPropertyDefinition.class);

    private final Class<A> type;
    private final Map<Method, A> annotations;

    private MethodAnnotationCache(Class<A> type) {
        this.type = type;
        this.annotations = new HashMap<>();
    }

    synchronized A getAnnotation(Method method) {
        A annotation = annotations.get(method); // NOSONAR
        if (annotation == null) {
            // let's not try again getting the annotation if it wasn't found in the first place
            if (!annotations.containsKey(method)) { // NOSONAR
                annotation = InheritedMethodAnnotationHelper.getAnnotation(method, type);
            }
            annotations.put(method, annotation);
        }
        return annotation;
    }
}