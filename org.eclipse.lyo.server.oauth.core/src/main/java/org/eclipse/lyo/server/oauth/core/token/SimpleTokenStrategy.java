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
package org.eclipse.lyo.server.oauth.core.token;

import java.io.IOException;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

import org.eclipse.lyo.server.oauth.core.OAuthRequest;

import net.oauth.OAuth;
import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;
import net.oauth.OAuthProblemException;

/**
 * A simple strategy for generating and validating tokens. Generates random
 * tokens and stores them in memory. Tokens are only good for the life of the
 * process.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class SimpleTokenStrategy implements TokenStrategy {
	private final static int REQUEST_TOKEN_MAX_ENTIRES = 500;
	private final static int ACCESS_TOKEN_MAX_ENTRIES = 5000;
	
	// key is request token, value is boolean indicating if the token is
	// authorized
	private Map<String, Boolean> requestTokens;

	// key is access token, value is consumer key
	private Map<String, String> accessTokens;

	// key is token, value is token secret
	private Map<String, String> tokenSecrets;

	public SimpleTokenStrategy() {
		this(REQUEST_TOKEN_MAX_ENTIRES, ACCESS_TOKEN_MAX_ENTRIES);
	}

	public SimpleTokenStrategy(int requestTokenMaxCount, int accessTokenMaxCount) {
		requestTokens = new LRUCache<String, Boolean>(requestTokenMaxCount);
		accessTokens = new LRUCache<String, String>(accessTokenMaxCount);
		tokenSecrets = new LRUCache<String, String>(requestTokenMaxCount
				+ accessTokenMaxCount);
	}

	@Override
	public void generateRequestToken(OAuthRequest oAuthRequest) {
		OAuthAccessor accessor = oAuthRequest.getAccessor();
		accessor.requestToken = generateTokenString();
		accessor.tokenSecret = generateTokenString();
		synchronized (requestTokens) {
			requestTokens.put(accessor.requestToken, Boolean.FALSE); // not yet authorized
		}
		synchronized (tokenSecrets) {
			tokenSecrets.put(accessor.requestToken, accessor.tokenSecret);
		}
	}

	@Override
	public String validateRequestToken(OAuthRequest oAuthRequest) throws OAuthException, IOException {
		String requestToken = oAuthRequest.getMessage().getToken();
		if (!requestTokens.containsKey(requestToken)) {
			throw new OAuthProblemException(OAuth.Problems.TOKEN_REJECTED);
		}
		
		return requestToken;
	}

	@Override
	public void markRequestTokenAuthorized(HttpServletRequest httpRequest,
			String requestToken) {
		synchronized (requestTokens) {
			requestTokens.put(requestToken, Boolean.TRUE);
		}
	}

	@Override
	public boolean isRequestTokenAuthorized(HttpServletRequest httpRequest,
			String requestToken) {
		synchronized (requestTokens) {
			Boolean authorized = requestTokens.get(requestToken);
			if (authorized == null) {
				return false;
			}

			return authorized.booleanValue();
		}
	}

	@Override
	public void generateAccessToken(OAuthRequest oAuthRequest) throws OAuthProblemException,
			IOException {
		// Remove the old request token.
		OAuthAccessor accessor = oAuthRequest.getAccessor();
		String requestToken = oAuthRequest.getMessage().getToken();
		synchronized (requestTokens) {
			if (!isRequestTokenAuthorized(oAuthRequest.getHttpRequest(),
					requestToken)) {
				throw new OAuthProblemException(
						OAuth.Problems.ADDITIONAL_AUTHORIZATION_REQUIRED);
			}

			requestTokens.remove(requestToken);
		}

		// Generate a new access token.
		accessor.accessToken = generateTokenString();
		synchronized (requestTokens) {
			accessTokens.put(accessor.accessToken,
					accessor.consumer.consumerKey);
		}

		// Remove the old token secret and create a new one for this access
		// token.
		accessor.tokenSecret = generateTokenString();
		synchronized (tokenSecrets) {
			tokenSecrets.remove(requestToken);
			tokenSecrets.put(accessor.accessToken, accessor.tokenSecret);
		}

		accessor.requestToken = null;
	}

	@Override
	public void validateAccessToken(OAuthRequest oAuthRequest)
			throws OAuthException, IOException {
		synchronized (accessTokens) {
			String actualValue = accessTokens.get(oAuthRequest.getMessage()
					.getToken());
			if (!oAuthRequest.getConsumer().consumerKey.equals(actualValue)) {
				throw new OAuthProblemException(OAuth.Problems.TOKEN_REJECTED);
			}
		}
	}

	@Override
	public String getTokenSecret(HttpServletRequest httpRequest, String token)
			throws OAuthProblemException {
		synchronized (tokenSecrets) {
			String tokenSecret = tokenSecrets.get(token);
			if (tokenSecret == null) {
				// It's possible the token secret was purged from the LRU cache,
				// or the token is just not recognized. Either way, we can
				// consider the token rejected.
				throw new OAuthProblemException(OAuth.Problems.TOKEN_REJECTED);
			}
			return tokenSecret;
		}
	}
	
	/**
	 * Creates a unique, random string to use for tokens.
	 * 
	 * @return the random string
	 */
	protected String generateTokenString() {
		return UUID.randomUUID().toString();
	}
}
