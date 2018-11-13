/*******************************************************************************
 * Copyright (c) 2011, 2012 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.server.oauth.core.utils;

import javax.servlet.http.HttpServletResponse;

/**
 * Holds a status code and error message for an error response.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
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
