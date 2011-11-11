/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
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
package org.eclipse.lyo.samples.bugzilla.utils;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.ws.commons.util.Base64;
import org.apache.ws.commons.util.Base64.DecodingException;
import org.eclipse.lyo.samples.bugzilla.Credentials;
import org.eclipse.lyo.samples.bugzilla.exception.RestException;
import org.eclipse.lyo.samples.bugzilla.exception.UnauthroziedException;
import org.eclipse.lyo.samples.bugzilla.resources.Error;

/**
 * Utilities for working with HTTP requests and responses.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class HttpUtils {

	public static final String AUTHORIZATION_HEADER = "Authorization";
	public static final String WWW_AUTHENTICATE_HEADER = "WWW-Authenticate";
	private static final String BASIC_AUTHORIZATION_PREFIX = "Basic ";
	private static final String WWW_AUTHENTICATE_HEADER_VALUE = BASIC_AUTHORIZATION_PREFIX
			+ "realm=\"Bugzilla\"";

	/**
	 * Gets the credentials from an HTTP request.
	 * 
	 * @param request
	 *            the request
	 * @return the Bugzilla credentials or <code>null</code> if the request did
	 *         not contain an <code>Authorization</code> header
	 * @throws UnauthroziedException
	 *             on problems reading the credentials from the
	 *             <code>Authorization</code> request header
	 */
	public static Credentials getCredentials(HttpServletRequest request)
			throws UnauthroziedException {
		String authorizationHeader = request.getHeader(AUTHORIZATION_HEADER);
		if (authorizationHeader == null || "".equals(authorizationHeader)) {
			return null;
		}
	
		Credentials credentials = new Credentials();
		if (!authorizationHeader.startsWith(HttpUtils.BASIC_AUTHORIZATION_PREFIX)) {
			throw new UnauthroziedException(
					"Only basic access authentication is supported.");
		}
		
		String encodedString = authorizationHeader.substring(HttpUtils.BASIC_AUTHORIZATION_PREFIX.length());
		try {
			String unencodedString = new String(Base64.decode(encodedString), "UTF-8");
			int seperator = unencodedString.indexOf(':');
			if (seperator == -1) {
				throw new UnauthroziedException("Invalid Authorization header value.");
			}
			
			credentials.setUsername(unencodedString.substring(0, seperator));
			credentials.setPassword(unencodedString.substring(seperator + 1));
		} catch (DecodingException e) {
			throw new UnauthroziedException("Username and password not Base64 encoded.");
		} catch (UnsupportedEncodingException e) {
			throw new UnauthroziedException("Invalid Authorization header value.");
		}
		
		return credentials;
	}

	public static void sendUnauthorizedResponse(HttpServletResponse response,
			UnauthroziedException e) throws IOException {
		response.setHeader(WWW_AUTHENTICATE_HEADER,
				WWW_AUTHENTICATE_HEADER_VALUE);
		sendErrorResponse(response, e);
	}

	private static void sendErrorResponse(HttpServletResponse response,
			RestException e) throws IOException {
		response.setContentType("application/rdf+xml");
		RdfUtils.sendErrorResponse(response, Error
				.fromRestException(e),
				RdfUtils.JENA_LANG_ABBREVIATED_RDF_XML);
	}
}
