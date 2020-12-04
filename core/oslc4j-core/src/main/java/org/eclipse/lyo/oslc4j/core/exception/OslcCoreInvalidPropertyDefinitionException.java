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

import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;

public final class OslcCoreInvalidPropertyDefinitionException extends OslcCoreApplicationException {
	private static final long serialVersionUID = 6043500589743612250L;

	private static final String MESSAGE_KEY = "InvalidPropertyDefinitionException";

	private final Method				 method;
	private final OslcPropertyDefinition oslcPropertyDefinition;
	private final Class<?>				 resourceClass;

	public OslcCoreInvalidPropertyDefinitionException(final Class<?> resourceClass, final Method method, final OslcPropertyDefinition oslcPropertyDefinition) {
		super(MESSAGE_KEY, new Object[] {resourceClass.getName(), method.getName(), oslcPropertyDefinition.value()});

		this.method					= method;
		this.oslcPropertyDefinition = oslcPropertyDefinition;
		this.resourceClass			= resourceClass;
	}

	public Method getMethod() {
		return method;
	}

	public OslcPropertyDefinition getOslcPropertyDefinition()
	{
		return oslcPropertyDefinition;
	}

	public Class<?> getResourceClass() {
		return resourceClass;
	}
}
