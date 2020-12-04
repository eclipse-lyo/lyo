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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

/**
 * ModelUnmarshallingException is an exception that occurs during
 * {@link org.apache.jena.rdf.model.Model}
 * transformation.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public class ModelUnmarshallingException extends Exception {
    public ModelUnmarshallingException() {
        super();
    }

    public ModelUnmarshallingException(final String message) {
        super(message);
    }

    public ModelUnmarshallingException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public ModelUnmarshallingException(final Throwable cause) {
        super(cause);
    }

    protected ModelUnmarshallingException(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean stackTrace) {
        super(message, cause, enableSuppression, stackTrace);
    }
}
