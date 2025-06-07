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
package org.eclipse.lyo.oslc4j.core.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

/**
 * Indicate which RDF: collection type should be used for representing
 * in RDF the multi-valued property
 */
@Documented
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OslcRdfCollectionType {
    String RDF_LIST = "List";
    String RDF_SEQ = "Seq";
    String RDF_ALT = "Alt";
    String RDF_BAG = "Bag";

    /**
     * Namespace URI.
     */
    String namespaceURI() default OslcConstants.RDF_NAMESPACE;

    /**
     * Prefix for the namespace.
     */
    String collectionType() default "List";
}
