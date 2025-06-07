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

public final class OslcCoreInvalidPropertyTypeException extends OslcCoreApplicationException {
    private static final long serialVersionUID = -6885356099037103377L;

    private static final String MESSAGE_KEY = "InvalidPropertyTypeException";

    private final Method method;
    private final Class<?> resourceClass;
    private final Class<?> returnType;

    public OslcCoreInvalidPropertyTypeException(
            final Class<?> resourceClass, final Method method, final Class<?> returnType) {
        super(
                MESSAGE_KEY,
                new Object[] {resourceClass.getName(), method.getName(), returnType.getName()});

        this.method = method;
        this.resourceClass = resourceClass;
        this.returnType = returnType;
    }

    public Method getMethod() {
        return method;
    }

    public Class<?> getResourceClass() {
        return resourceClass;
    }

    public Class<?> getReturnType() {
        return returnType;
    }
}
