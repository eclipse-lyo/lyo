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
package org.eclipse.lyo.oslc4j.core.exception;

import java.lang.reflect.Method;
import org.eclipse.lyo.oslc4j.core.model.Representation;

public final class OslcCoreInvalidRepresentationException extends OslcCoreApplicationException {
    private static final long serialVersionUID = -3803394959079264170L;

    private static final String MESSAGE_KEY = "InvalidRepresentationException";

    private final Method method;
    private final Representation representation;
    private final Class<?> resourceClass;

    public OslcCoreInvalidRepresentationException(
            final Class<?> resourceClass,
            final Method method,
            final Representation representation) {
        super(
                MESSAGE_KEY,
                new Object[] {
                    resourceClass.getName(), method.getName(), representation.toString()
                });

        this.method = method;
        this.representation = representation;
        this.resourceClass = resourceClass;
    }

    public Method getMethod() {
        return method;
    }

    public Representation getRepresentation() {
        return representation;
    }

    public Class<?> getResourceClass() {
        return resourceClass;
    }
}
