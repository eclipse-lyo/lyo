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

import javax.servlet.http.HttpServletResponse;

import net.oauth.OAuthProblemException;
import net.oauth.OAuthValidator;
import net.oauth.SimpleOAuthValidator;
import net.oauth.http.HttpMessage;

import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;
import org.eclipse.lyo.server.oauth.core.token.SimpleTokenStrategy;
import org.eclipse.lyo.server.oauth.core.token.TokenStrategy;

/**
 * Manages the OAuth provider configuration, including the validator, consumer store, and
 * token strategy.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class OAuthConfiguration {
	private OAuthValidator validator;
	private TokenStrategy tokenStrategy;
	private ConsumerStore consumerStore = null;
	private Application application = null;
	private boolean v1_0Allowed = true;

	private static final OAuthConfiguration instance = new OAuthConfiguration();

	public static OAuthConfiguration getInstance() {
		return instance;
	}

	private OAuthConfiguration() {
		validator = new SimpleOAuthValidator();
		tokenStrategy = new SimpleTokenStrategy();
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
	 * @throws ConsumerStoreException on errors initializing the consumer registry
	 */
	public void setConsumerStore(ConsumerStore consumerStore) throws ConsumerStoreException {
		this.consumerStore = consumerStore;
	}

	public Application getApplication() throws OAuthProblemException {
		if (application == null) {
			OAuthProblemException e = new OAuthProblemException();
			e.setParameter(HttpMessage.STATUS_CODE,
					HttpServletResponse.SC_SERVICE_UNAVAILABLE);
			throw e;
		}

		return application;
	}

	public void setApplication(Application application) {
		this.application = application;
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
