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

public final class OslcCoreInvalidPropertyTypeException extends OslcCoreApplicationException {
    private static final long serialVersionUID = -6885356099037103377L;

    private static final String MESSAGE_KEY = "InvalidPropertyTypeException";

    private final Method   method;
    private final Class<?> resourceClass;
	private final Class<?> returnType;

	public OslcCoreInvalidPropertyTypeException(final Class<?> resourceClass, final Method method, final Class<?> returnType) {
        super(MESSAGE_KEY, new Object[] {resourceClass.getName(), method.getName(), returnType.getName()});

        this.method        = method;
        this.resourceClass = resourceClass;
        this.returnType    = returnType;
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