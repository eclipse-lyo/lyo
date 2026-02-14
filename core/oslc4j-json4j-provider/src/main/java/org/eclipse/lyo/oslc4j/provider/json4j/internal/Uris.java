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

import java.net.URI;
import java.net.URISyntaxException;

public final class Uris {

    /**
     * Creates a URI from a String value form.
     * Tries to 'normalize' the URI if the string contains spaces.
     *
     * @param value
     *            the String Url to normalize
     * @return the normalized URI
     * @throws URISyntaxException
     *             if an URI cannot be created from the String Url
     */
    // typically used for RMM 7.0 or Polarion Compacts having spaces in query parameters
    public static URI create(String value) throws URISyntaxException {

        try {
            return new URI(value);
        } catch (URISyntaxException e) {
            // spaces are invalid in URIs
            if (value.contains(" ")) { //$NON-NLS-1$
                return new URI(value.replace(' ', '+'));
            } else {
                throw e;
            }
        }
    }

    private Uris() {
    }

}
