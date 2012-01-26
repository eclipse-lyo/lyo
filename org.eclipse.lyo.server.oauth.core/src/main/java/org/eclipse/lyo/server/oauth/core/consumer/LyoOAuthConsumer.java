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
package org.eclipse.lyo.server.oauth.core.consumer;

import net.oauth.OAuthConsumer;
import net.oauth.OAuthServiceProvider;

/**
 * An OAuth consumer with extra properties, including a name and a trusted flag.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class LyoOAuthConsumer extends OAuthConsumer {
	private static final long serialVersionUID = 7634987410903334464L;

	public enum OAuthVersion { OAUTH_1_0, OAUTH_1_0A };
	
	private String name;
	private boolean trusted = false;

	/*
	 * Assume 1.0 until we learn otherwise. This can be determined if the
	 * consumer passes an oauth_callback parameter when asking for a request
	 * token.
	 */
	private OAuthVersion oAuthVersion = OAuthVersion.OAUTH_1_0;
	
	public LyoOAuthConsumer(String consumerKey, String consumerSecret) {
		super(null, consumerKey, consumerSecret, null);
	}

	public LyoOAuthConsumer(String callbackURL, String consumerKey,
			String consumerSecret) {
		super(callbackURL, consumerKey, consumerSecret, null);
	}

	public LyoOAuthConsumer(String callbackURL, String consumerKey,
			String consumerSecret, OAuthServiceProvider serviceProvider) {
		super(callbackURL, consumerKey, consumerSecret, serviceProvider);
	}

	/**
	 * Gets the name of the consumer, which might be shown in the login dialog
	 * and other user interfaces.
	 * 
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name of the consumer, which might be shown in the login dialog
	 * and other user interfaces.
	 * 
	 * @param name
	 *            the name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Answers if this consumer is trusted. If a consumer is trusted, a login
	 * prompt might be skipped if the user is already authenticated with this
	 * web page.
	 * 
	 * @return true if the consumer is trusted, false otherwise
	 */
	public boolean isTrusted() {
		return trusted;
	}

	/**
	 * Sets if this consumer is trusted. If a consumer is trusted, a login
	 * prompt might be skipped if the user is already authenticated with this
	 * web page.
	 * 
	 * @param trusted true if the consumer is trusted, false otherwise
	 */
	public void setTrusted(boolean trusted) {
		this.trusted = trusted;
	}

	/**
	 * Gets the OAuth version that the consumer supports.
	 * 
	 * @return the OAuth version
	 */
	public OAuthVersion getOAuthVersion() {
		return oAuthVersion;
	}

	/**
	 * Sets the OAuth version that the consumer supports. This should be set by
	 * the OAuth service depending on whether the consumer specified an
	 * oauth_callback parameter when asking for a request token.
	 * 
	 * @param oAuthVersion
	 *            the OAuth version
	 */
	public void setOAuthVersion(OAuthVersion oAuthVersion) {
		this.oAuthVersion = oAuthVersion;
	}
}
