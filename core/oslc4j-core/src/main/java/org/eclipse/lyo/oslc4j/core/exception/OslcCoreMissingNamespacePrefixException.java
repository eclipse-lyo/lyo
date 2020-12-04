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

public final class OslcCoreMissingNamespacePrefixException extends OslcCoreApplicationException {
	private static final long serialVersionUID = 1587802829795702844L;

	private static final String MESSAGE_KEY = "MissingNamespacePrefixException";

	private final String prefix;

	public OslcCoreMissingNamespacePrefixException(final String prefix) {
		super(MESSAGE_KEY, new Object[] {prefix});

		this.prefix = prefix;
	}

	public String getPrefix() {
		return prefix;
	}
}