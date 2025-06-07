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
package org.eclipse.lyo.shacl;

import java.net.URI;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

public enum DataType {
    Boolean(OslcConstants.XML_NAMESPACE + "boolean"),
    DateTime(OslcConstants.XML_NAMESPACE + "dateTime"),
    Date(OslcConstants.XML_NAMESPACE + "date"),
    Decimal(OslcConstants.XML_NAMESPACE + "decimal"),
    Double(OslcConstants.XML_NAMESPACE + "double"),
    Float(OslcConstants.XML_NAMESPACE + "float"),
    Integer(OslcConstants.XML_NAMESPACE + "integer"),
    String(OslcConstants.XML_NAMESPACE + "string"),
    XMLLiteral(OslcConstants.RDF_NAMESPACE + "XMLLiteral"),
    Resource(OslcConstants.OSLC_CORE_NAMESPACE + "Resource");

    private final String uri;

    DataType(final String uri) {
        this.uri = uri;
    }

    @Override
    public String toString() {
        return uri;
    }

    public static DataType fromString(final String string) {
        final DataType[] values = DataType.values();
        for (final DataType value : values) {
            if (value.uri.equals(string)) {
                return value;
            }
        }
        return null;
    }

    public static DataType fromURI(final URI uri) {
        return fromString(uri.toString());
    }
}
