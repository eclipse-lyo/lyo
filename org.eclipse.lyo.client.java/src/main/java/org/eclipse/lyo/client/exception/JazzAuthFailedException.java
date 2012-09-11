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
 *     Michael Fiedler     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.exception;


/**
 * Exception indicating a Jazz authentication or credentials problem
 *
 */
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
