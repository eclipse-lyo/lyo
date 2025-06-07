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
package org.eclipse.lyo.oslc4j.core;

/**
 * Represents properties whose value is not valid for the declared
 * datatype. This enables clients to at least see the raw value and
 * preserve the value when updating the resource with PUT. Strict
 * datatypes must be disabled.
 *
 * @see "AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES"
 */
public class UnparseableLiteral {
    private String rawValue;
    private String datatype;

    public UnparseableLiteral(String rawValue, String datatype) {
        this.setDatatype(datatype);
        this.setRawValue(rawValue);
    }

    public String getRawValue() {
        return this.rawValue;
    }

    public String getDatatype() {
        return this.datatype;
    }

    public void setRawValue(String value) {
        this.rawValue = value;
    }

    public void setDatatype(String value) {
        this.datatype = value;
    }

    public String toString() {
        return "unparseable literal: \"" + this.rawValue + "\"^^<" + this.datatype + ">";
    }
}
