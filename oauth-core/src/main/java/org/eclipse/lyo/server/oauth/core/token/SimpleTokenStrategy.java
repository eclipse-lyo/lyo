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
import net.oauth.OAuthMessage;
import net.oauth.OAuthProblemException;

/**
 * A simple strategy for generating and validating tokens. Generates random
 * tokens and stores them in memory. Tokens are only good for the life of the
 * process. Least recently used tokens are invalidated when cached limits are
 * reached.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
public class SimpleTokenStrategy implements TokenStrategy {
	private final static int REQUEST_TOKEN_MAX_ENTIRES = 500;
	private final static int ACCESS_TOKEN_MAX_ENTRIES = 5000;
	
	/**
	 * Holds information associated with a request token such as the callback
	 * URL and OAuth verification code.
	 * 
	 * @author Samuel Padgett <spadgett@us.ibm.com>
	 */
	protected class RequestTokenData {
		private String consumerKey;
		private boolean authorized;
		private String callback;
		private String verificationCode;
		
		public RequestTokenData(String consumerKey) {
			this.consumerKey = consumerKey;
			this.authorized = false;
			this.callback = null;
		}

		public RequestTokenData(String consumerKey, String callback) {
			this.consumerKey = consumerKey;
			this.authorized = false;
			this.callback = callback;
		}
		
		public String getConsumerKey() {
			return consumerKey;
		}
		
		public void setConsumerKey(String consumerKey) {
			this.consumerKey = consumerKey;
		}
		
		public boolean isAuthorized() {
			return authorized;
		}
		
		public void setAuthorized(boolean authorized) {
			this.authorized = authorized;
		}
		
		public String getCallback() {
			return callback;
		}
		
		public void setCallback(String callback) {
			this.callback = callback;
		}
		
		public String getVerificationCode() {
			return verificationCode;
		}
		
		public void setVerificationCode(String verificationCode) {
			this.verificationCode = verificationCode;
		}
	}
	
	// key is request token string, value is RequestTokenData
	private Map<String, RequestTokenData> requestTokens;

	// key is access token, value is consumer key
	private Map<String, String> accessTokens;

	// key is token, value is token secret
	private Map<String, String> tokenSecrets;

	/**
	 * Constructs a SimpleTokenStrategy using the defaults for cache limits on request and access tokens.
	 * 
	 * @see SimpleTokenStrategy#SimpleTokenStrategy(int, int)
	 */
	public SimpleTokenStrategy() {
		this(REQUEST_TOKEN_MAX_ENTIRES, ACCESS_TOKEN_MAX_ENTRIES);
	}

	/**
	 * Constructs a SimpleTokenStrategy with cache limits on the number of
	 * request and access tokens. Least recently used tokens are invalidated
	 * when cache limits are reached.
	 * 
	 * @param requestTokenMaxCount
	 *            the maximum number of request tokens to track
	 * @param accessTokenMaxCount
	 *            the maximum number of access tokens to track
	 */
	public SimpleTokenStrategy(int requestTokenMaxCount, int accessTokenMaxCount) {
		requestTokens = new LRUCache<String, RequestTokenData>(requestTokenMaxCount);
		accessTokens = new LRUCache<String, String>(accessTokenMaxCount);
		tokenSecrets = new LRUCache<String, String>(requestTokenMaxCount
				+ accessTokenMaxCount);
	}

	@Override
	public void generateRequestToken(OAuthRequest oAuthRequest)
			throws IOException {
		OAuthAccessor accessor = oAuthRequest.getAccessor();
		accessor.requestToken = generateTokenString();
		accessor.tokenSecret = generateTokenString();
		String callback = oAuthRequest.getMessage()
				.getParameter(OAuth.OAUTH_CALLBACK);
		synchronized (requestTokens) {
			requestTokens.put(accessor.requestToken, new RequestTokenData(
					accessor.consumer.consumerKey, callback));
		}
		synchronized (tokenSecrets) {
			tokenSecrets.put(accessor.requestToken, accessor.tokenSecret);
		}
	}

	@Override
	public String validateRequestToken(HttpServletRequest httpRequest,
			OAuthMessage message) throws OAuthException, IOException {
		return getRequestTokenData(message.getToken()).getConsumerKey();
	}

	@Override
	public String getCallback(HttpServletRequest httpRequest,
			String requestToken) throws OAuthProblemException {
		return getRequestTokenData(requestToken).getCallback();
	}

	@Override
	public void markRequestTokenAuthorized(HttpServletRequest httpRequest,
			String requestToken) throws OAuthProblemException {
		getRequestTokenData(requestToken).setAuthorized(true);
	}

	@Override
	public boolean isRequestTokenAuthorized(HttpServletRequest httpRequest,
			String requestToken) throws OAuthProblemException {
		return getRequestTokenData(requestToken).isAuthorized();
	}

	@Override
	public String generateVerificationCode(HttpServletRequest httpRequest,
			String requestToken) throws OAuthProblemException {
		String verificationCode = generateTokenString();
		getRequestTokenData(requestToken).setVerificationCode(verificationCode);
		
		return verificationCode;
	}

	@Override
	public void validateVerificationCode(OAuthRequest oAuthRequest)
			throws OAuthException, IOException {
		String verificationCode = oAuthRequest.getMessage().getParameter(
				OAuth.OAUTH_VERIFIER);
		if (verificationCode == null) {
			throw new OAuthProblemException(
					OAuth.Problems.OAUTH_PARAMETERS_ABSENT);
		}

		RequestTokenData tokenData = getRequestTokenData(oAuthRequest);
		if (!verificationCode.equals(tokenData.getVerificationCode())) {
			throw new OAuthProblemException(
					OAuth.Problems.OAUTH_PARAMETERS_REJECTED);
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
		synchronized (accessTokens) {
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

	/**
	 * Gets the request token data from this OAuth request.
	 * 
	 * @param oAuthRequest
	 *            the OAuth request
	 * @return the request token data
	 * @throws OAuthProblemException
	 *             if the request token is invalid
	 * @throws IOException
	 *             on reading OAuth parameters
	 */
	protected RequestTokenData getRequestTokenData(OAuthRequest oAuthRequest)
			throws OAuthProblemException, IOException {
		return getRequestTokenData(oAuthRequest.getMessage().getToken());
	}

	/**
	 * Gets the request token data for this request token.
	 * 
	 * @param requestToken
	 *            the request token string
	 * @return the request token data
	 * @throws OAuthProblemException
	 *             if the request token is invalid
	 */
	protected RequestTokenData getRequestTokenData(String requestToken)
			throws OAuthProblemException {
		synchronized (requestTokens) {
			RequestTokenData tokenData = requestTokens.get(requestToken);
			if (tokenData == null) {
				throw new OAuthProblemException(OAuth.Problems.TOKEN_REJECTED);
			}
			return tokenData;
		}
	}
}
