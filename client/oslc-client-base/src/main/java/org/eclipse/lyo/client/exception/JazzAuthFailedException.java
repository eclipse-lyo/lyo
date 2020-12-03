/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-1.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client.exception;


/**
 * Thrown when the HTTP status code for the response from the Jazz server
 * indicates an error in the request.  Contains the status code and URL of
 * the server.
 *
 */
@SuppressWarnings("serial")
public class JazzAuthFailedException extends OslcClientApplicationException {

	private static final String MESSAGE_KEY = "JazzAuthFailedException";

	private final String user;
	private final String jazzUrl;

	public JazzAuthFailedException(final String user, final String jazzUrl) {
		super(MESSAGE_KEY, new Object[] {user, jazzUrl});
		this.user = user;
		this.jazzUrl = jazzUrl;
	}

	public String getUser() {
		return user;
	}

	public String getJazzUrl() {
		return jazzUrl;
	}


}
