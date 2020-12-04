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
package org.eclipse.lyo.client.oslc;

import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;

@Deprecated
public class OAuthRedirectException extends OAuthException {
	private static final long serialVersionUID = -8602283923593056515L;

	private String redirectURL;
	private OAuthAccessor accessor;

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
