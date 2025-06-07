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
package org.eclipse.lyo.client.exception;

/**
 * Exception indicating a Jazz authentication or credentials problem
 *
 */
@SuppressWarnings("serial")
public class ResourceNotFoundException extends OslcClientApplicationException {

    private static final String MESSAGE_KEY = "ResourceNotFoundException";

    private final String resource;
    private final String value;

    public ResourceNotFoundException(final String resource, final String value) {
        super(MESSAGE_KEY, new Object[] {resource, value});
        this.resource = resource;
        this.value = value;
    }

    public String getResource() {
        return resource;
    }

    public String getValue() {
        return value;
    }
}
