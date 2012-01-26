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

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;

import net.oauth.OAuth;
import net.oauth.OAuth.Parameter;
import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.OAuthProblemException;
import net.oauth.OAuthValidator;
import net.oauth.server.OAuthServlet;

import org.eclipse.lyo.server.oauth.core.Authentication;
import org.eclipse.lyo.server.oauth.core.AuthenticationException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.OAuthRequest;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerRegistry;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;
import org.eclipse.lyo.server.oauth.core.token.TokenStrategy;

/**
 * Issues OAuth request tokens, handles authentication, and then exchanges
 * request tokens for access tokens based on the OAuth configuration set in the
 * {@link OAuthConfiguration} singleton.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see <a href="http://tools.ietf.org/html/rfc5849">The OAuth 1.0 Protocol</a>
 */
@Path("/oauth")
public class OAuthService {

	@Context
	protected HttpServletRequest httpRequest;

	@Context
	protected HttpServletResponse httpResponse;

	@GET
	@Path("/requestToken")
	public Response doGetRequestToken() throws IOException, ServletException {
		return doPostRequestToken();
	}
	
	/**
	 * Responds with a request token and token secret.
	 * 
	 * @return the response
	 * @throws IOException
	 *             on I/O errors
	 * @throws ServletException
	 *             on servlet errors
	 */
	@POST
	@Path("/requestToken")
	public Response doPostRequestToken() throws IOException, ServletException {
		try {
			OAuthRequest oAuthRequest = validateRequest();

			// Generate the token.
			OAuthConfiguration.getInstance().getTokenStrategy()
					.generateRequestToken(oAuthRequest);

			// Check for OAuth 1.0a authentication.
			boolean callbackConfirmed = confirmCallback(oAuthRequest);
			
			// Respond to the consumer.
			OAuthAccessor accessor = oAuthRequest.getAccessor();
			return respondWithToken(accessor.requestToken,
					accessor.tokenSecret, callbackConfirmed);
		} catch (OAuthException e) {
			return respondWithOAuthProblem(e);
		}
	}

	protected boolean confirmCallback(OAuthRequest oAuthRequest)
			throws OAuthException {
		boolean callbackConfirmed = OAuthConfiguration
				.getInstance()
				.getTokenStrategy()
				.getCallback(httpRequest,
						oAuthRequest.getAccessor().requestToken) != null;
		if (callbackConfirmed) {
			oAuthRequest.getConsumer().setOAuthVersion(
					LyoOAuthConsumer.OAuthVersion.OAUTH_1_0A);
		} else {
			if (!OAuthConfiguration.getInstance().isV1_0Allowed()) {
				throw new OAuthProblemException(
						OAuth.Problems.OAUTH_PARAMETERS_ABSENT);
			}

			oAuthRequest.getConsumer().setOAuthVersion(
					LyoOAuthConsumer.OAuthVersion.OAUTH_1_0);
		}

		return callbackConfirmed;
	}

	/*
	 * TODO: Give providers a way to show their own branded login page.
	 */

	/**
	 * Responds with a web page to log in.
	 * 
	 * @return the response
	 * @throws IOException
	 *             on I/O errors
	 * @throws ServletException
	 *             on internal errors validating the request
	 */
	@GET
	@Path("/authorize")
	public Response authorize() throws ServletException, IOException {
		try {
			/*
			 * Check that the request token is valid and determine what consumer
			 * it's for. The OAuth spec does not require that consumers pass the
			 * consumer key to the authorization page, so we must track this in
			 * the TokenStrategy implementation.
			 */
			OAuthMessage message = OAuthServlet.getMessage(httpRequest, null);
			OAuthConfiguration config = OAuthConfiguration.getInstance();
			String consumerKey = config.getTokenStrategy()
					.validateRequestToken(httpRequest, message);
			
			LyoOAuthConsumer consumer = ConsumerRegistry.getInstance()
					.getConsumer(consumerKey);

			// Pass some data to the JSP.
			httpRequest.setAttribute("requestToken", message.getToken());
			httpRequest.setAttribute("consumerName", consumer.getName());
			httpRequest.setAttribute("callback",
					getCallbackURL(message, consumer));
			boolean callbackConfirmed =
					consumer.getOAuthVersion() == LyoOAuthConsumer.OAuthVersion.OAUTH_1_0A;
			httpRequest.setAttribute("callbackConfirmed", new Boolean(
					callbackConfirmed));

			Authentication auth = config.getAuthentication();
			if (auth == null) {
				return Response.status(Status.SERVICE_UNAVAILABLE)
						.type(MediaType.TEXT_PLAIN)
						.entity("OAuth service is not configured.").build();
			}

			// The application name is displayed on the OAuth login page.
			httpRequest.setAttribute("applicationName",
					auth.getApplicationName());

			// Show the login page.
			httpRequest.getRequestDispatcher("/oauth/login.jsp").forward(
					httpRequest, httpResponse);
			
			return null;
		} catch (OAuthException e) {
			return respondWithOAuthProblem(e);
		}
	}

	private String getCallbackURL(OAuthMessage message,
			LyoOAuthConsumer consumer) throws IOException, OAuthException {
		String callback = null;
		switch (consumer.getOAuthVersion()) {
		case OAUTH_1_0:
			if (!OAuthConfiguration.getInstance().isV1_0Allowed()) {
				throw new OAuthProblemException(OAuth.Problems.VERSION_REJECTED);
			}

			// If this is OAuth 1.0, the callback should be a request parameter.
			callback = message.getParameter(OAuth.OAUTH_CALLBACK);
			break;

		case OAUTH_1_0A:
			// If this is OAuth 1.0a, the callback was passed when the consumer
			// asked for a request token.
			String requestToken = message.getToken();
			callback = OAuthConfiguration.getInstance().getTokenStrategy()
					.getCallback(httpRequest, requestToken);
		}

		if (callback == null) {
			return null;
		}

		UriBuilder uriBuilder = UriBuilder.fromUri(callback)
				.queryParam(OAuth.OAUTH_TOKEN, message.getToken());
		if (consumer.getOAuthVersion() == LyoOAuthConsumer.OAuthVersion.OAUTH_1_0A) {
			String verificationCode = OAuthConfiguration.getInstance()
					.getTokenStrategy()
					.generateVerificationCode(httpRequest, message.getToken());
			uriBuilder.queryParam(OAuth.OAUTH_VERIFIER, verificationCode);
		}
		
		return uriBuilder.build().toString();
	}

	/**
	 * Validates the ID and password on the authorization form. This is intended
	 * to be invoked by an XHR on the login page.
	 * 
	 * @return the response, 409 if login failed or 204 if successful
	 */
	@POST
	@Path("/login")
	public Response login(@FormParam("id") String id,
			@FormParam("password") String password,
			@FormParam("requestToken") String requestToken) {
		Authentication auth = OAuthConfiguration.getInstance()
				.getAuthentication();

		try {
			auth.login(httpRequest, id, password);
		} catch (AuthenticationException e) {
			String message = e.getMessage();
			if (message == null || "".equals(message)) {
				message = "Incorrect username or password.";
			}
			return Response.status(Status.CONFLICT).entity(message)
					.type(MediaType.TEXT_PLAIN).build();
		}

		try {
			OAuthConfiguration.getInstance().getTokenStrategy()
					.markRequestTokenAuthorized(httpRequest, requestToken);
		} catch (OAuthException e) {
			return Response.status(Status.CONFLICT)
					.entity("Request token invalid.")
					.type(MediaType.TEXT_PLAIN).build();
		}
		
		return Response.noContent().build();
	}

	@GET
	@Path("/accessToken")
	public Response doGetAccessToken() throws IOException, ServletException {
		return doPostAccessToken();
	}
	
	/**
	 * Responds with an access token and token secret for valid OAuth requests.
	 * The request must be signed and the request token valid.
	 * 
	 * @return the response
	 * @throws IOException
	 *             on I/O errors
	 * @throws ServletException
	 *             on servlet errors
	 */
	@POST
	@Path("/accessToken")
	public Response doPostAccessToken() throws IOException, ServletException {
		try {
			// Validate the request is signed and check that the request token
			// is valid.
			OAuthRequest oAuthRequest = validateRequest();
			OAuthConfiguration config = OAuthConfiguration.getInstance();
			TokenStrategy strategy = config.getTokenStrategy();
			strategy.validateRequestToken(httpRequest,
					oAuthRequest.getMessage());

			// The verification code MUST be passed in the request if this is
			// OAuth 1.0a.
			if (!config.isV1_0Allowed()
					|| oAuthRequest.getConsumer().getOAuthVersion() == LyoOAuthConsumer.OAuthVersion.OAUTH_1_0A) {
				strategy.validateVerificationCode(oAuthRequest);
			}
			
			// Generate a new access token for this accessor.
			strategy.generateAccessToken(oAuthRequest);
			
			// Send the new token and secret back to the consumer.
			OAuthAccessor accessor = oAuthRequest.getAccessor();
			return respondWithToken(accessor.accessToken, accessor.tokenSecret);
		} catch (OAuthException e) {
			return respondWithOAuthProblem(e);
		}
	}

	/**
	 * Validates this is a known consumer and the request is valid using
	 * {@link OAuthValidator#validateMessage(net.oauth.OAuthMessage, OAuthAccessor)}
	 * . Does <b>not</b> check for any tokens
	 * 
	 * @return an OAuthRequest
	 * @throws OAuthException
	 *             if the request fails validation
	 * @throws IOException
	 *             on I/O errors
	 */
	protected OAuthRequest validateRequest() throws OAuthException, IOException {
		OAuthRequest oAuthRequest = new OAuthRequest(httpRequest);
		try {
			OAuthValidator validator = OAuthConfiguration.getInstance()
					.getValidator();
			validator.validateMessage(oAuthRequest.getMessage(),
					oAuthRequest.getAccessor());
		} catch (URISyntaxException e) {
			throw new WebApplicationException(e, Status.INTERNAL_SERVER_ERROR);
		}

		return oAuthRequest;
	}

	protected Response respondWithToken(String token, String tokenSecret)
			throws IOException {
		return respondWithToken(token, tokenSecret, false);
	}

	protected Response respondWithToken(String token, String tokenSecret,
			boolean callbackConfirmed) throws IOException {
		List<Parameter> oAuthParameters = OAuth.newList(OAuth.OAUTH_TOKEN,
				token, OAuth.OAUTH_TOKEN_SECRET, tokenSecret);
		if (callbackConfirmed) {
			oAuthParameters.add(new Parameter(OAuth.OAUTH_CALLBACK_CONFIRMED,
					"true"));
		}
		
		String responseBody = OAuth.formEncode(oAuthParameters);
		return Response.ok(responseBody)
				.type(MediaType.APPLICATION_FORM_URLENCODED).build();
	}

	protected Response respondWithOAuthProblem(OAuthException e)
			throws IOException, ServletException {
		OAuthServlet.handleException(httpResponse, e, OAuthConfiguration
				.getInstance().getRealm());
		return Response.status(Status.UNAUTHORIZED).build();
	}
}
