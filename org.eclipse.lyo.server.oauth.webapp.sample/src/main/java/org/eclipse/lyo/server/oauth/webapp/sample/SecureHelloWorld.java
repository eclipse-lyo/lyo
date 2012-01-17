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
package org.eclipse.lyo.server.oauth.webapp.sample;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import net.oauth.OAuthException;
import net.oauth.server.OAuthServlet;

import org.eclipse.lyo.server.oauth.core.Authentication;
import org.eclipse.lyo.server.oauth.core.AuthenticationException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.OAuthRequest;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;

/**
 * A simple OAuth example using the Lyo OAuth provider framework.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
@Path("/hello")
public class SecureHelloWorld implements ServletContextListener {

	@Context
	private HttpServletRequest httpRequest;

	@Context
	private HttpServletResponse httpResponse;

	/**
	 * Initialize the OAuth provider when the webapp loads.
	 * 
	 * @param event
	 *            the context event
	 */
	@Override
	public void contextInitialized(ServletContextEvent event) {
		OAuthConfiguration config = OAuthConfiguration.getInstance();

		// The realm used in 401 unauthorized responses.
		config.setRealm("Hello");

		// Validates a user's ID and password.
		config.setAuthentication(new Authentication() {
			@Override
			public void login(HttpServletRequest request, String id,
					String password) throws AuthenticationException {
				// For this example, accept all ID/password combination unless
				// the password is "bogus."
				if ("bogus".equals(password)) {
					throw new AuthenticationException("Invalid ID or password.");
				}
			}

			@Override
			public String getApplicationName() {
				// Display name for this application.
				return "Hello World";
			}
		});

		try {
			// The consumers.
			config.setConsumerStore(new ConsumerStore() {
				@Override
				public Collection<LyoOAuthConsumer> load() throws IOException {
					// Define one consumer with key "key" and secret "secret".
					LyoOAuthConsumer consumer = new LyoOAuthConsumer("key", "secret");
					consumer.setName("Hello World Consumer");
					return Collections.singletonList(consumer);
				}
			});
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void contextDestroyed(ServletContextEvent event) {
	}

	@GET
	public Response helloWorld() throws IOException, ServletException {
		try {
			OAuthRequest request = new OAuthRequest(httpRequest);
			request.validate();
		} catch (OAuthException e) {
			// Request failed validation. Send an unauthorized response.
			OAuthServlet.handleException(httpResponse, e, OAuthConfiguration
					.getInstance().getRealm());
			return Response.status(Status.UNAUTHORIZED).build();
		}

		// Validation passed. Respond with the secret message.
		return Response.ok("Hello World!").type(MediaType.TEXT_PLAIN).build();
	}
}
