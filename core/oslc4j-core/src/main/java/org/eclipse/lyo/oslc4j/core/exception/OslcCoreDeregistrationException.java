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

public final class OslcCoreDeregistrationException extends OslcCoreApplicationException {
    private static final long serialVersionUID = 2969548886287595367L;

    private static final String MESSAGE_KEY = "DeregistrationException";

    private final String responseMessage;
    private final URI serviceProviderURI;
    private final int statusCode;

    public OslcCoreDeregistrationException(
            final URI serviceProviderURI, final int statusCode, final String responseMessage) {
        super(
                MESSAGE_KEY,
                new Object[] {serviceProviderURI.toString(), statusCode, responseMessage});

        this.responseMessage = responseMessage;
        this.serviceProviderURI = serviceProviderURI;
        this.statusCode = statusCode;
    }

    public String getResponseMessage() {
        return responseMessage;
    }

    public URI getServiceProviderURI() {
        return serviceProviderURI;
    }

    public int getStatusCode() {
        return statusCode;
    }
}
