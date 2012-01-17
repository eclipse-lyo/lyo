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

import javax.servlet.http.HttpServletRequest;

/**
 * Handles authentication with the backend system.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see OAuthConfiguration#setAuthentication(Authentication)
 */
public interface Authentication {
	/**
	 * Gets the name of the application to show in the login dialog.
	 * 
	 * @return the application name
	 */
	public String getApplicationName();

	/**
	 * Authenticates with the application. On errors, throws an
	 * {@link AuthenticationException}.
	 * 
	 * @param request
	 *            the servlet request
	 * @param id
	 *            the user's ID
	 * @param password
	 *            the user's password
	 * @throws AuthenticationException
	 *             if authentication fails
	 */
	public void login(HttpServletRequest request, String id, String password)
			throws AuthenticationException;
}
