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

import java.net.URI;

public enum ValueType {
    Boolean(OslcConstants.XML_NAMESPACE + "boolean"),
    DateTime(OslcConstants.XML_NAMESPACE + "dateTime"),
    Date(OslcConstants.XML_NAMESPACE + "date"),
    Decimal(OslcConstants.XML_NAMESPACE + "decimal"),
    Double(OslcConstants.XML_NAMESPACE + "double"),
    Float(OslcConstants.XML_NAMESPACE + "float"),
    Integer(OslcConstants.XML_NAMESPACE + "integer"),
    String(OslcConstants.XML_NAMESPACE + "string"),
    XMLLiteral(OslcConstants.RDF_NAMESPACE + "XMLLiteral"),
    Resource(OslcConstants.OSLC_CORE_NAMESPACE + "Resource"),
    LocalResource(OslcConstants.OSLC_CORE_NAMESPACE + "LocalResource");
    //	AnyResource(OslcConstants.OSLC_CORE_ENUM_NAMESPACE + "AnyResource"); // AnyResource not
    // supported by OSLC4J

    private final String uri;

    ValueType(final String uri) {
        this.uri = uri;
    }

    @Override
    public String toString() {
        return uri;
    }

    public static ValueType fromString(final String string) {
        final ValueType[] values = ValueType.values();
        for (final ValueType value : values) {
            if (value.uri.equals(string)) {
                return value;
            }
        }
        return null;
    }

    public static ValueType fromURI(final URI uri) {
        return fromString(uri.toString());
    }
}
