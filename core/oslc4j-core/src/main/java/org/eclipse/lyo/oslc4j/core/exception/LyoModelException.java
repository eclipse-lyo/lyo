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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

package org.eclipse.lyo.oslc4j.core.exception;

/**
 * An unchecked exception to indicate a problem with (un)marshalling an RDF graph model.
 */
public class LyoModelException extends RuntimeException {

    public LyoModelException() {
    }

    public LyoModelException(final String message) {
        super(message);
    }

    public LyoModelException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public LyoModelException(final Throwable cause) {
        super(cause);
    }

    public LyoModelException(final String message, final Throwable cause, final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
