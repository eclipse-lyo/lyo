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
 *	  Kevin Bauer       - initial API and implementation
 *  
 *******************************************************************************/
package org.eclipse.lyo.client.oslc;

import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;


public class OAuthRedirectException extends OAuthException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8602283923593056515L;
	
	private String redirectURL;
	private OAuthAccessor accessor;
	
	/**
	 * @param redirectURL
	 * @param requestToken
	 * @param tokenSecret
	 */
	public OAuthRedirectException(String redirectURL, OAuthAccessor accessor) {
		this.redirectURL = redirectURL;
		this.accessor = accessor;
	}

	/**
	 * @return the redirectURL
	 */
	public String getRedirectURL() {
		return redirectURL;
	}

	/**
	 * @param redirectURL the redirectURL to set
	 */
	public void setRedirectURL(String redirectURL) {
		this.redirectURL = redirectURL;
	}

	/**
	 * @return the accessor
	 */
	public OAuthAccessor getAccessor() {
		return accessor;
	}

	/**
	 * @param accessor the accessor to set
	 */
	public void setAccessor(OAuthAccessor accessor) {
		this.accessor = accessor;
	}
}
