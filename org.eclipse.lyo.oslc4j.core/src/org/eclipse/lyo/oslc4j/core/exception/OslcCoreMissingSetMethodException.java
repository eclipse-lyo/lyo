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

public final class OslcCoreMissingSetMethodException extends OslcCoreApplicationException {
    private static final long serialVersionUID = 4513570830160136304L;

    private static final String MESSAGE_KEY = "MissingSetMethodException";

    private final Class<?> resourceClass;
    private final Method   getMethod;

	public OslcCoreMissingSetMethodException(final Class<?> resourceClass, final Method getMethod, final Exception exception) {
        super(MESSAGE_KEY, new Object[] {resourceClass.getName(), getMethod.getName()}, exception);

        this.getMethod     = getMethod;
        this.resourceClass = resourceClass;
    }

    public Method getGetMethod()
    {
        return getMethod;
    }

    public Class<?> getResourceClass()
    {
        return resourceClass;
    }
}