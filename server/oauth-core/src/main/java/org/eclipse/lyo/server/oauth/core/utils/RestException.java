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
package org.eclipse.lyo.server.oauth.core.utils;

import javax.servlet.http.HttpServletResponse;

/**
 * Holds a status code and error message for an error response.
 * 
 * @author Samuel Padgett
 */
public class RestException extends Exception {
	private int statusCode;
	private String message;
	
	public RestException(int statusCode, String message) {
		this.statusCode = statusCode;
		this.message = message;
	}
	
	public RestException(Throwable t) {
		this(t, HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	}
	
	public RestException(Throwable t, int statusCode) {
		super(t);
		this.message = t.getMessage();
		this.statusCode = statusCode;
	}

	public int getStatusCode() {
		return statusCode;
	}
	
	public void setStatusCode(int statusCode) {
		this.statusCode = statusCode;
	}
	
	public String getMessage() {
		return message;
	}
	
	public void setMessage(String message) {
		this.message = message;
	}
}
