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
package org.eclipse.lyo.oslc4j.core.exception;

import java.lang.reflect.Method;

import org.eclipse.lyo.oslc4j.core.model.ValueType;

public final class OslcCoreInvalidValueTypeException extends OslcCoreApplicationException {
	private static final long serialVersionUID = -989384752764371312L;

	private static final String MESSAGE_KEY = "InvalidValueTypeException";

	private final Method	method;
	private final Class<?>	resourceClass;
	private final ValueType valueType;

	public OslcCoreInvalidValueTypeException(final Class<?> resourceClass, final Method method, final ValueType valueType) {
		super(MESSAGE_KEY, new Object[] {resourceClass.getName(), method.getName(), valueType.toString()});

		this.method		   = method;
		this.resourceClass = resourceClass;
		this.valueType	   = valueType;
	}

	public Method getMethod() {
		return method;
	}

	public Class<?> getResourceClass() {
		return resourceClass;
	}

	public ValueType getValueType() {
		return valueType;
	}
}