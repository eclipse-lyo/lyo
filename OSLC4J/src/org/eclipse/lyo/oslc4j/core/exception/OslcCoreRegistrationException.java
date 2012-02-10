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

import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;

public final class OslcCoreRegistrationException extends OslcCoreApplicationException {
    private static final long serialVersionUID = 2094758752309893978L;

    private static final String MESSAGE_KEY = "RegistrationException";

    private final String          responseMessage;
	private final ServiceProvider serviceProvider;
	private final int             statusCode;

	public OslcCoreRegistrationException(final ServiceProvider serviceProvider, final int statusCode, final String responseMessage) {
        super(MESSAGE_KEY, new Object[] {serviceProvider.getTitle(), Integer.valueOf(statusCode), responseMessage});

        this.responseMessage = responseMessage;
        this.serviceProvider = serviceProvider;
        this.statusCode      = statusCode;
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