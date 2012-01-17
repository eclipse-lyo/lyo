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

import net.oauth.OAuth;
import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;
import net.oauth.OAuthValidator;
import net.oauth.server.OAuthServlet;

import org.eclipse.lyo.server.oauth.core.Authentication;
import org.eclipse.lyo.server.oauth.core.AuthenticationException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.OAuthRequest;
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
	public Response getRequestToken() throws IOException, ServletException {
		try {

			OAuthRequest oAuthRequest = validateRequest();
			OAuthConfiguration.getInstance()
					.getTokenStrategy().generateRequestToken(oAuthRequest);

			OAuthAccessor accessor = oAuthRequest.getAccessor();
			return respondWithToken(accessor.requestToken, accessor.tokenSecret);

		} catch (OAuthException e) {
			return respondWithOAuthProblem(e);
		}
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
			// Check that the request is valid.
			OAuthRequest oAuthRequest = validateRequest();
			OAuthConfiguration config = OAuthConfiguration.getInstance();			
			String requestToken = config.getTokenStrategy()
					.validateRequestToken(oAuthRequest);
			
			// Pass some data to the JSP.
			httpRequest.setAttribute("requestToken", requestToken);
			httpRequest.setAttribute("consumerName", oAuthRequest.getConsumer()
					.getName());
			httpRequest.setAttribute("callback",
					oAuthRequest.getConsumer().callbackURL);

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

		OAuthConfiguration.getInstance().getTokenStrategy()
				.markRequestTokenAuthorized(httpRequest, requestToken);
		return Response.noContent().build();
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
	public Response getAccessToken() throws IOException, ServletException {
		try {

			OAuthRequest oAuthRequest = validateRequest();

			TokenStrategy strategy = OAuthConfiguration.getInstance()
					.getTokenStrategy();
			strategy.validateRequestToken(oAuthRequest);
			strategy.generateAccessToken(oAuthRequest);

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
		String responseBody = OAuth.formEncode(OAuth.newList(OAuth.OAUTH_TOKEN,
				token, OAuth.OAUTH_TOKEN_SECRET, tokenSecret));

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
