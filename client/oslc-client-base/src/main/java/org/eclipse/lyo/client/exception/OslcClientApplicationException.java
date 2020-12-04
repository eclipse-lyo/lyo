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
package org.eclipse.lyo.client.exception;

/**
 * Base class for all application exceptions.
 */
public abstract class OslcClientApplicationException extends Exception {
    private static final long serialVersionUID = -5933150329026674184L;

    public OslcClientApplicationException(final String messageKey, final Object[] args) {
        super(MessageExtractor.getMessage(messageKey, args));
    }

    public OslcClientApplicationException(final String messageKey, final Object[] args, final Throwable t) {
        super(MessageExtractor.getMessage(messageKey, args), t);
    }
}
