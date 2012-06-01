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
 * <code>BugzillaException</code> indicates that Bugzilla has returned a fault rather 
 * than the expected return value for a method. It wraps the <code>XmlRpcException</code>
 * which caused the error.
 *
 *
 */
public class SharepointException extends Exception {

	/**
	 * Eclipse-generated SUID
	 */
	private static final long serialVersionUID = -5427986526722263296L;

	/**
	 * Public constructor which calls super()
	 * @param message A customized error message describing the issue
	 * @param cause The nested XmlRpcException cause
	 */
	public SharepointException(String message, Throwable cause) {
		super(message, cause);
	}
	
	/**
	 * Constructs a new {@link BugzillaException} with the specified summary
	 * @param message A short, descriptive message of the error
	 */
	public SharepointException(String message) {
		super(message);
	}
}
