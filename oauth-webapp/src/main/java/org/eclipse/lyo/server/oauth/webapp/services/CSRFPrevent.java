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
package org.eclipse.lyo.server.oauth.webapp.services;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

/**
 * Checks requests to see if they have the right X-CSRF-Prevent header values. 
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class CSRFPrevent {
	private static final String CSRF_PREVENT_HEADER = "X-CSRF-Prevent"; //$NON-NLS-1$
	
	public static void check(HttpServletRequest httpRequest) {
		String csrfPrevent = httpRequest.getHeader(CSRF_PREVENT_HEADER);
		String sessionId = httpRequest.getSession().getId();
		if (!sessionId.equals(csrfPrevent)) {
			throw new WebApplicationException(Response.status(Status.FORBIDDEN)
					.entity("Request denied due to possible CSRF attack.").type(MediaType.TEXT_PLAIN).build());
		}
	}
}
