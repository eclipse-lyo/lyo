/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
package org.eclipse.lyo.server.oauth.core;

/**
 * An exception indicating that authentication failed.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see Application#login(String, char[])
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
