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

import java.net.URI;

public final class OslcCoreRelativeURIException extends OslcCoreApplicationException {
	private static final long serialVersionUID = -1238625637837216499L;

	private static final String MESSAGE_KEY = "RelativeURIException";

	private final String   methodName;
	private final URI	   relativeURI;
	private final Class<?> resourceClass;

	public OslcCoreRelativeURIException(final Class<?> resourceClass, final String methodName, final URI relativeURI) {
		super(MESSAGE_KEY, new Object[] {resourceClass.getName(), methodName, relativeURI.toString()});

		this.methodName	   = methodName;
		this.relativeURI   = relativeURI;
		this.resourceClass = resourceClass;
	}

	public String getMethodName() {
		return methodName;
	}

	public URI getRelativeURI() {
		return relativeURI;
	}

	public Class<?> getResourceClass() {
		return resourceClass;
	}
}