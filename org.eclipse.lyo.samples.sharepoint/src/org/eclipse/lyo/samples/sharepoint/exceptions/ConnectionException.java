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
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.samples.sharepoint.exceptions;

/**
 * <code>ConnectionException</code> is thrown any time there is an error trying to access
 * the Bugzilla installation over the network. It may be a network-related problem such as
 * a timeout or malformed URL, or it may be an underlying XML-RPC exception, but it
 * generally indicates that any further Bugzilla calls will fail.
 * 
 * ConnectionException will always be a wrapper for a nested <code>Exception</code> which
 * indicates the cause of the error.
 * 
 *
 */
public class ConnectionException extends Exception {

	/**
	 * Eclipse-generated SUID
	 */
	private static final long serialVersionUID = 2957676868743832929L;

	/**
	 * Public constructor which calls super()
	 * @param message A custom error message describing the issue
	 * @param cause The root cause of the exception
	 */
	public ConnectionException(String message, Throwable cause) {
		super(message, cause);
	}
	
}
