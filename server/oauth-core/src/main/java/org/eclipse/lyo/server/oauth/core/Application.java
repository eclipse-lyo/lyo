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

import javax.servlet.http.HttpServletRequest;

/**
 * Handles authentication with the backend system.
 * 
 * @author Samuel Padgett
 * @see OAuthConfiguration#setApplication(Application)
 */
public interface Application {
	/**
	 * Gets the name of the application to show in the login dialog.
	 * 
	 * @return the application name
	 */
	public String getName();

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

	/**
	 * Determines if the user is already authenticated with the application. If
	 * so, the OAuth provider can show a different authorization dialog that
	 * doesn't require a login.
	 * 
	 * @param request
	 *            the servlet request
	 * @return if the user is already logged in for this session
	 */
	public boolean isAuthenticated(HttpServletRequest request);
	
	/**
	 * Determines if the current session is an admin session. If so, the user
	 * will be able to approve, edit, and delete OAuth consumers.
	 * 
	 * @param request
	 *            the HTTP request
	 * @return if this is an admin session
	 */
	public boolean isAdminSession(HttpServletRequest request);

	/**
	 * Gets the realm to be included in OAuth problem responses.
	 * 
	 * @param request
	 *            the HTTP request
	 * @return the realm
	 */
	public String getRealm(HttpServletRequest request);
}
