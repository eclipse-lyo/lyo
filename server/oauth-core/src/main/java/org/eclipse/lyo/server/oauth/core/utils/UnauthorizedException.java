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
package org.eclipse.lyo.server.oauth.core.utils;

import jakarta.servlet.http.HttpServletResponse;

/**
 * Corresponds to an HTTP 401 response.
 *
 * @author Samuel Padgett
 */
public class UnauthorizedException extends RestException {
    public UnauthorizedException() {
        super(
                HttpServletResponse.SC_UNAUTHORIZED,
                "You must authenticate with your tool for this request.");
    }

    public UnauthorizedException(String message) {
        super(HttpServletResponse.SC_UNAUTHORIZED, message);
    }

    public UnauthorizedException(Throwable t) {
        super(t, HttpServletResponse.SC_UNAUTHORIZED);
    }
}
