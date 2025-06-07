package org.eclipse.lyo.store;

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

/**
 * StoreAccessException is an exception for read/write problems with the backing triplestore or
 * dataset.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.13
 */
public class StoreAccessException extends Exception {
    public StoreAccessException() {
        super();
    }

    public StoreAccessException(final String message) {
        super(message);
    }

    public StoreAccessException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public StoreAccessException(final Throwable cause) {
        super(cause);
    }

    protected StoreAccessException(
            final String message,
            final Throwable cause,
            final boolean enableSuppression,
            final boolean stackTrace) {
        super(message, cause, enableSuppression, stackTrace);
    }
}
