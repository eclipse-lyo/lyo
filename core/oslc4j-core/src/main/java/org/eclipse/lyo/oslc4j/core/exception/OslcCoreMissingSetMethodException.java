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

public final class OslcCoreMissingSetMethodException extends OslcCoreApplicationException {
	private static final long serialVersionUID = 4513570830160136304L;

	private static final String MESSAGE_KEY = "MissingSetMethodException";

	private final Class<?> resourceClass;
	private final Method   getMethod;

	public OslcCoreMissingSetMethodException(final Class<?> resourceClass, final Method getMethod, final Exception exception) {
		super(MESSAGE_KEY, new Object[] {resourceClass.getName(), getMethod.getName()}, exception);

		this.getMethod	   = getMethod;
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