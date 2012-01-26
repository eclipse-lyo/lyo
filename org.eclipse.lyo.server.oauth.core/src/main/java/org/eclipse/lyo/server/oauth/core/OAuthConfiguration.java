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

import java.io.IOException;

import net.oauth.OAuthValidator;
import net.oauth.SimpleOAuthValidator;

import org.eclipse.lyo.server.oauth.core.consumer.ConsumerRegistry;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore;
import org.eclipse.lyo.server.oauth.core.token.SimpleTokenStrategy;
import org.eclipse.lyo.server.oauth.core.token.TokenStrategy;

/**
 * Manages the OAuth provider configuration, including the validator, consumer store, and
 * token strategy.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class OAuthConfiguration {
	private String realm;
	private OAuthValidator validator;
	private TokenStrategy tokenStrategy;
	private ConsumerStore consumerStore = null;
	private Authentication authentication = null;
	private boolean v1_0Allowed = true;

	private static final OAuthConfiguration instance = new OAuthConfiguration();

	public static OAuthConfiguration getInstance() {
		return instance;
	}

	private OAuthConfiguration() {
		realm = "Lyo";
		validator = new SimpleOAuthValidator();
		tokenStrategy = new SimpleTokenStrategy();
	}

	/**
	 * Gets the realm to be included in OAuth problem responses.
	 * 
	 * @return the realm
	 */
	public String getRealm() {
		return realm;
	}

	/**
	 * Sets the realm to be included in OAuth problem responses.
	 * 
	 * @param realm the realm
	 */
	public void setRealm(String realm) {
		this.realm = realm;
	}

	/**
	 * Gets the OAuth validator for validating request signatures.
	 * 
	 * @return the validator
	 */
	public OAuthValidator getValidator() {
		return validator;
	}

	/**
	 * Sets the OAuth validator for validating request signatures.
	 * 
	 * @param validator the validator
	 */
	public void setValidator(OAuthValidator validator) {
		this.validator = validator;
	}

	/**
	 * Gets the strategy used to generate and verify OAuth tokens.
	 * 
	 * @return the token strategy
	 */
	public TokenStrategy getTokenStrategy() {
		return tokenStrategy;
	}

	/**
	 * Sets the strategy used to generate and verify OAuth tokens.
	 * 
	 * @param tokenStrategy the strategy
	 */
	public void setTokenStrategy(TokenStrategy tokenStrategy) {
		this.tokenStrategy = tokenStrategy;
	}

	/**
	 * Gets the store used for managing consumers.
	 * 
	 * @return the consumer store
	 */
	public ConsumerStore getConsumerStore() {
		return consumerStore;
	}

	/**
	 * Sets the store used for managing consumers.
	 * 
	 * @param consumerStore the consumer store
	 * @throws IOException 
	 */
	public void setConsumerStore(ConsumerStore consumerStore) throws IOException {
		this.consumerStore = consumerStore;
		ConsumerRegistry.getInstance().init(consumerStore);
	}

	public Authentication getAuthentication() {
		return authentication;
	}

	public void setAuthentication(Authentication authentication) {
		this.authentication = authentication;
	}

	/**
	 * Is OAuth version 1.0 allowed, or do we require 1.0a?
	 * 
	 * @return true if version 1.0 is allowed
	 * @see <a href="http://oauth.net/advisories/2009-1/">OAuth Security Advisory: 2009.1</a>
	 */
	public boolean isV1_0Allowed() {
		return v1_0Allowed;
	}

	/**
	 * Sets if we allow OAuth 1.0.
	 * 
	 * @param allowed
	 *            true to allow OAuth version 1.0 requests or false to require
	 *            OAuth version 1.0a
	 * @see <a href="http://oauth.net/advisories/2009-1/">OAuth Security Advisory: 2009.1</a>
	 */
	public void setV1_0Allowed(boolean allowed) {
		this.v1_0Allowed = allowed;
	}
}
