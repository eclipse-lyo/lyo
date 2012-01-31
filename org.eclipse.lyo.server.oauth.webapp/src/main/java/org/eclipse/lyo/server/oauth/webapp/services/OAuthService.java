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
import java.net.InetAddress;
import java.net.URISyntaxException;
import java.util.List;
import java.util.UUID;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
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

import org.apache.wink.json4j.JSON;
import org.apache.wink.json4j.JSONException;
import org.apache.wink.json4j.JSONObject;
import org.eclipse.lyo.server.oauth.core.Application;
import org.eclipse.lyo.server.oauth.core.AuthenticationException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.OAuthRequest;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;
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
			
			LyoOAuthConsumer consumer = OAuthConfiguration.getInstance()
					.getConsumerStore().getConsumer(consumerKey);

			// Pass some data to the JSP.
			httpRequest.setAttribute("requestToken", message.getToken());
			httpRequest.setAttribute("consumerName", consumer.getName());
			httpRequest.setAttribute("callback",
					getCallbackURL(message, consumer));
			boolean callbackConfirmed =
					consumer.getOAuthVersion() == LyoOAuthConsumer.OAuthVersion.OAUTH_1_0A;
			httpRequest.setAttribute("callbackConfirmed", new Boolean(
					callbackConfirmed));

			// The application name is displayed on the OAuth login page.
			httpRequest.setAttribute("applicationName",
					config.getApplication().getName());

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
		try {
			OAuthConfiguration.getInstance().getApplication()
					.login(httpRequest, id, password);
		} catch (OAuthException e) {
			return Response.status(Status.SERVICE_UNAVAILABLE).build();
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
	 * Generates a provisional consumer key. This request must be later approved
	 * by an administrator.
	 * 
	 * @return a JSON response with the provisional key
	 * @throws IOException
	 * @throws NullPointerException
	 * @see <a href="https://jazz.net/wiki/bin/view/Main/RootServicesSpecAddendum2">Jazz Root Services Spec Addendum2</a>
	 */
	@POST
	@Path("/requestKey")
	// Some consumers do not set an appropriate Content-Type header.
	//@Consumes({ MediaType.APPLICATION_JSON })
	@Produces({ MediaType.APPLICATION_JSON })
	public Response provisionalKey()
			throws NullPointerException, IOException {
		try {
			// Create the consumer from the request.
			JSONObject request = (JSONObject) JSON.parse(httpRequest
					.getInputStream());

			String name = null;
			if (request.has("name") && request.get("name") != null) {
				name = request.getString("name");
			}
			
			if (name == null || name.trim().equals("")) {
				name = getRemoteHost();
			}

			String secret = request.getString("secret");

			boolean trusted = false;
			if (request.has("trusted")) {
				trusted = "true".equals(request.getString("trusted"));
			}

			String key = UUID.randomUUID().toString();
			LyoOAuthConsumer consumer = new LyoOAuthConsumer(key, secret);
			consumer.setName(name);
			consumer.setProvisional(true);
			consumer.setTrusted(trusted);
			
			// Add the consumer to the store.
			OAuthConfiguration.getInstance().getConsumerStore().addConsumer(consumer);

			// Respond with the consumer key.
			JSONObject response = new JSONObject();
			response.put("key", key);
			
			return Response.ok(response.write()).build();
		} catch (JSONException e) {
			e.printStackTrace();
			return Response.status(Status.BAD_REQUEST).build();
		} catch (ConsumerStoreException e) {
			e.printStackTrace();
			return Response.status(Status.SERVICE_UNAVAILABLE)
					.type(MediaType.TEXT_PLAIN).entity(e.getMessage()).build();
		}
	}

	/**
	 * Shows the approval page for a single provisional consumer. Shows the
	 * consumer management page instead if no key is passed in.
	 * 
	 * @param key
	 *            the consumer
	 * @return the approve consumer page
	 * @throws ServletException
	 *             on errors showing the JSP
	 * @throws IOException
	 *             on errors showing the JSP
	 * @see #showConsumerKeyManagementPage()
	 */
	@GET
	@Path("/approveKey")
	@Produces({ MediaType.TEXT_HTML })
	public Response showApproveKeyPage(@QueryParam("key") String key)
			throws ServletException, IOException {
		if (key == null || "".equals(key)) {
			return showConsumerKeyManagementPage();
		}

		try {
			Application app = OAuthConfiguration.getInstance().getApplication();
			
			// The application name is displayed on approval page.
			httpRequest.setAttribute("applicationName", app.getName());

			if (!app.isAdminSession(httpRequest)) {
				return showAdminLogin();
			}

			LyoOAuthConsumer provisionalConsumer = OAuthConfiguration
					.getInstance().getConsumerStore().getConsumer(key);

			if (provisionalConsumer == null) {
				return Response.status(Status.BAD_REQUEST).build();
			}

			httpRequest.setAttribute("consumerName",
					provisionalConsumer.getName());
			httpRequest.setAttribute("consumerKey",
					provisionalConsumer.consumerKey);
			httpRequest
					.setAttribute("trusted", provisionalConsumer.isTrusted());
			final String dispatchTo = (provisionalConsumer.isProvisional()) ? "/oauth/approveKey.jsp"
					: "/oauth/keyAlreadyApproved.jsp";
			httpRequest.getRequestDispatcher(dispatchTo).forward(httpRequest,
					httpResponse);
			return null;

		} catch (ConsumerStoreException e) {
			e.printStackTrace();
			return Response.status(Status.CONFLICT).type(MediaType.TEXT_PLAIN)
					.entity(e.getMessage()).build();
		} catch (OAuthProblemException e) {
			return respondWithOAuthProblem(e);
		}
	}

	/**
	 * Shows the consumer management page, which allows administrator to approve
	 * or remove OAuth consumers.
	 * 
	 * @return the consumer management page
	 * @throws ServletException
	 *             on JSP errors
	 * @throws IOException
	 *             on JSP errors
	 */
	@GET
	@Path("/admin")
	public Response showConsumerKeyManagementPage() throws ServletException,
			IOException {
		try {
			Application app = OAuthConfiguration.getInstance().getApplication();

			httpRequest.setAttribute("applicationName", app.getName());
			if (!app.isAdminSession(httpRequest)) {
				return showAdminLogin();
			}
		} catch (OAuthException e) {
			return Response.status(Status.SERVICE_UNAVAILABLE).build();
		}

		httpRequest.getRequestDispatcher("/oauth/manage.jsp").forward(
				httpRequest, httpResponse);
		return null;
	}
	
	/**
	 * Validates that the ID and password are for an administrator. This is used
	 * by the admin login page to protect the OAuth administration pages.
	 * 
	 * @return the response, 409 if login failed or 204 if successful
	 */
	@POST
	@Path("/adminLogin")
	public Response login(@FormParam("id") String id,
			@FormParam("password") String password) {
		try {
			Application app = OAuthConfiguration.getInstance().getApplication();
			app.login(httpRequest, id, password);

			if (app.isAdminSession(httpRequest)) {
				return Response.noContent().build();
			}

			return Response.status(Status.CONFLICT)
					.entity("The user '" + id + "' is not an administrator.")
					.type(MediaType.TEXT_PLAIN).build();
		} catch (OAuthException e) {
			return Response.status(Status.SERVICE_UNAVAILABLE).build();
		} catch (AuthenticationException e) {
			String message = e.getMessage();
			if (message == null || "".equals(message)) {
				message = "Incorrect username or password.";
			}
			return Response.status(Status.CONFLICT).entity(message)
					.type(MediaType.TEXT_PLAIN).build();
		}
	}

	/**
	 * Validates this is a known consumer and the request is valid using
	 * {@link OAuthValidator#validateMessage(net.oauth.OAuthMessage, OAuthAccessor)}.
	 * Does <b>not</b> check for any tokens.
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
		try {
			OAuthServlet.handleException(httpResponse, e, OAuthConfiguration
					.getInstance().getApplication().getRealm(httpRequest));
		} catch (OAuthProblemException serviceUnavailableException) {
			return Response.status(Status.SERVICE_UNAVAILABLE).build();
		}
		
		return Response.status(Status.UNAUTHORIZED).build();
	}

	private String getRemoteHost() {
		try {
			// Try to get the hostname of the consumer.
			return InetAddress.getByName(httpRequest.getRemoteHost())
					.getCanonicalHostName();
		} catch (Exception e) {
			/*
			 * Not fatal, and we shouldn't fail here. Fall back to returning
			 * ServletRequest.getRemoveHost(). It might be the IP address, but
			 * that's better than nothing.
			 */
			return httpRequest.getRemoteHost();
		}
	}

	private Response showAdminLogin() throws ServletException, IOException {
		StringBuffer callback = httpRequest.getRequestURL();
		String query = httpRequest.getQueryString();
		if (query != null) {
			callback.append('?');
			callback.append(query);
		}
		httpRequest.setAttribute("callback", callback.toString());
		httpRequest.getRequestDispatcher("/oauth/adminLogin.jsp").forward(
				httpRequest, httpResponse);
		return null;
	}
}
