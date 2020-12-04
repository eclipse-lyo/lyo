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
package org.eclipse.lyo.client.exception;


/**
 * Thrown when an HTTP response comes back from the Jazz server with an HTTP response header
 * X-com-ibm-team-repository-web-auth-msg with a value of "authfailed". The server sends this
 * when the user could not be authenticated. The exception will contain
 * the username and the URL of the server.
 *
 */
@SuppressWarnings("serial")
public class JazzAuthErrorException extends OslcClientApplicationException {

	private static final String MESSAGE_KEY = "JazzAuthErrorException";

	private final int status;
	private final String jazzUrl;

	public JazzAuthErrorException(final int status, final String jazzUrl) {
		super(MESSAGE_KEY, new Object[] {new Integer(status), jazzUrl});
		this.status = status;
		this.jazzUrl = jazzUrl;
	}

	public int getStatus() {
		return status;
	}

	public String getJazzUrl() {
		return jazzUrl;
	}


}
