/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.exception;

import java.lang.reflect.Method;

public final class OslcCoreMisusedOccursException extends OslcCoreApplicationException {
    private static final long serialVersionUID = -7018793552909566125L;

    private static final String MESSAGE_KEY = "MisusedOccursException";

    private final Method     method;
    private final Class<?>   resourceClass;

	public OslcCoreMisusedOccursException(final Class<?> resourceClass, final Method method) {
        super(MESSAGE_KEY, new Object[] {resourceClass.getName(), method.getName()});

        this.method        = method;
        this.resourceClass = resourceClass;
    }

	public Method getMethod() {
        return method;
    }

    public Class<?> getResourceClass() {
        return resourceClass;
    }
}