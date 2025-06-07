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

public enum Occurs {
    ExactlyOne(OslcConstants.OSLC_CORE_NAMESPACE + "Exactly-one"),
    ZeroOrOne(OslcConstants.OSLC_CORE_NAMESPACE + "Zero-or-one"),
    ZeroOrMany(OslcConstants.OSLC_CORE_NAMESPACE + "Zero-or-many"),
    OneOrMany(OslcConstants.OSLC_CORE_NAMESPACE + "One-or-many");

    private String uri;

    Occurs(final String uri) {
        this.uri = uri;
    }

    @Override
    public String toString() {
        return uri;
    }

    public static Occurs fromString(final String string) {
        final Occurs[] values = Occurs.values();
        for (final Occurs value : values) {
            if (value.uri.equals(string)) {
                return value;
            }
        }
        return null;
    }

    public static Occurs fromURI(final URI uri) {
        return fromString(uri.toString());
    }
}
