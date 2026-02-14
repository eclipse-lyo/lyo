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

import javax.xml.namespace.QName;

import com.sodius.oslc.core.provider.internal.NamespaceMappings;

/*
 * Common constants and methods for reading and writing Json content
 */
abstract class AbstractBuilder {
    static final String JSON_PROPERTY_DELIMITER = ":";

    static final String PREFIXES = "prefixes";
    static final String PROPERTY_ABOUT = "about";
    static final String PROPERTY_MEMBER = "member";
    static final String PROPERTY_RESOURCE = "resource";
    static final String PROPERTY_TITLE = "title";
    static final String PROPERTY_RESULTS = "results";
    static final String PROPERTY_RESPONSE_INFO = "responseInfo";
    static final String PROPERTY_TOTAL_COUNT = "totalCount";
    static final String PROPERTY_NEXT_PAGE = "nextPage";
    static final String PROPERTY_TYPE = "type";
    static final String PROPERTY_FIRST = "first";
    static final String PROPERTY_REST = "rest";
    static final String PROPERTY_NIL = "nil";

    static final String POSITIVE_INF = "INF";
    static final String NEGATIVE_INF = "-INF";
    static final String NOT_A_NUMBER = "NaN";

    private final NamespaceMappings namespaceMappings;

    AbstractBuilder(NamespaceMappings namespaceMappings) {
        this.namespaceMappings = namespaceMappings;
    }

    final NamespaceMappings getNamespaceMappings() {
        return namespaceMappings;
    }

    final String generateKey(QName qname) {
        return generateKey(qname.getNamespaceURI(), qname.getLocalPart(), qname.getPrefix());
    }

    final String generateKey(String namespace, String localPart, String prefix) {
        QName qname = namespaceMappings.generateQName(namespace, localPart, prefix);
        return qname.getPrefix() + JSON_PROPERTY_DELIMITER + qname.getLocalPart();
    }

}