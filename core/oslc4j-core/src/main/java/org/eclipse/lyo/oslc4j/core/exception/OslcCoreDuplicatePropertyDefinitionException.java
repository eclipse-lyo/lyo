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

import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;

public final class OslcCoreDuplicatePropertyDefinitionException extends OslcCoreApplicationException {
	private static final long serialVersionUID = 7981216864868316487L;

	private static final String MESSAGE_KEY = "DuplicatePropertyDefinitionException";

	private final OslcPropertyDefinition oslcPropertyDefinition;
	private final Class<?>				 resourceClass;

	public OslcCoreDuplicatePropertyDefinitionException(final Class<?> resourceClass, final OslcPropertyDefinition oslcPropertyDefinition) {
		super(MESSAGE_KEY, new Object[] {resourceClass.getName(), oslcPropertyDefinition.value()});

		this.oslcPropertyDefinition = oslcPropertyDefinition;
		this.resourceClass			= resourceClass;
	}

	public OslcPropertyDefinition getOslcPropertyDefinition()
	{
		return oslcPropertyDefinition;
	}

	public Class<?> getResourceClass() {
		return resourceClass;
	}
}