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

public final class OslcCoreMissingAnnotationException extends OslcCoreApplicationException {
    private static final long serialVersionUID = 247462012895583998L;

    private static final String MESSAGE_KEY = "MissingAnnotationException";

	private final Class<?> annotationClass;
	private final Class<?> resourceClass;

	public OslcCoreMissingAnnotationException(final Class<?> resourceClass, final Class<?> annotationClass) {
        super(MESSAGE_KEY, new Object[] {resourceClass.getName(), annotationClass.getName()});

        this.annotationClass = annotationClass;
        this.resourceClass   = resourceClass;
    }

	public Class<?> getAnnotationClass() {
		return annotationClass;
	}

	public Class<?> getResourceClass() {
		return resourceClass;
	}
}