/*******************************************************************************
 * Copyright (c) 2018 Yash Khatri
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
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/
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
