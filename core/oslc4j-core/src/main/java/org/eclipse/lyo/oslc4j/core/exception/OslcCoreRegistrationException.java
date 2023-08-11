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

import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;

public final class OslcCoreRegistrationException extends OslcCoreApplicationException {
	private static final long serialVersionUID = 2094758752309893978L;

	private static final String MESSAGE_KEY = "RegistrationException";

	private final String		  responseMessage;
	private final ServiceProvider serviceProvider;
	private final int			  statusCode;

	public OslcCoreRegistrationException(final ServiceProvider serviceProvider, final int statusCode, final String responseMessage) {
		super(MESSAGE_KEY, new Object[] {serviceProvider.getTitle(), statusCode, responseMessage});

		this.responseMessage = responseMessage;
		this.serviceProvider = serviceProvider;
		this.statusCode		 = statusCode;
	}

	public String getResponseMessage() {
		return responseMessage;
	}

	public ServiceProvider getServiceProvider() {
		return serviceProvider;
	}

	public int getStatusCode() {
		return statusCode;
	}
}
