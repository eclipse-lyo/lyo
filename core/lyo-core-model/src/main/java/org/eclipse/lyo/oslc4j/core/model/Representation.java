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

public enum Representation {
    Reference(OslcConstants.OSLC_CORE_NAMESPACE + "Reference"),
    Inline(OslcConstants.OSLC_CORE_NAMESPACE + "Inline");
    //	Either(OslcConstants.OSLC_CORE_NAMESPACE + "Either"); // Either not supported by OSLC4J

    private String uri;

    Representation(final String uri) {
        this.uri = uri;
    }

    @Override
    public String toString() {
        return uri;
    }

    public static Representation fromString(final String string) {
        final Representation[] values = Representation.values();
        for (final Representation value : values) {
            if (value.uri.equals(string)) {
                return value;
            }
        }
        return null;
    }

    public static Representation fromURI(final URI uri) {
        return fromString(uri.toString());
    }
}
