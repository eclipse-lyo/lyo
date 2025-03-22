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
package org.eclipse.lyo.server.oauth.core;

import jakarta.servlet.http.HttpServletRequest;

/**
 * An exception indicating that authentication failed.
 * 
 * @author Samuel Padgett
 * @see Application#login(HttpServletRequest, String, String)
 */
public class AuthenticationException extends Exception {
	private static final long serialVersionUID = -7357859796941279773L;

	public AuthenticationException() {
		super();
	}

	public AuthenticationException(String message, Throwable t) {
		super(message, t);
	}

	public AuthenticationException(String message) {
		super(message);
	}

	public AuthenticationException(Throwable t) {
		super(t);
	}
}
