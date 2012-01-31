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
import java.sql.SQLException;

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

import org.eclipse.lyo.server.oauth.consumerstore.RdfConsumerStore;
import org.eclipse.lyo.server.oauth.core.Application;
import org.eclipse.lyo.server.oauth.core.AuthenticationException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.OAuthRequest;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStoreException;

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
	 * The OAuth realm for this application.
	 */
	public static final String REALM = "Hello";
	
	/**
	 * Initialize the OAuth provider when the webapp loads.
	 * 
	 * @param event
	 *            the context event
	 */
	@Override
	public void contextInitialized(ServletContextEvent event) {
		OAuthConfiguration config = OAuthConfiguration.getInstance();

		// Validates a user's ID and password.
		config.setApplication(new Application() {
			@Override
			public void login(HttpServletRequest request, String id,
					String password) throws AuthenticationException {
				// For this example, accept all ID/password combination unless
				// the password is "bogus."
				if ("bogus".equals(password)) {
					throw new AuthenticationException("Invalid ID or password.");
				}
				
				request.getSession().setAttribute("admin", "admin".equals(id));
			}

			@Override
			public String getName() {
				return "Hello World";
			}

			@Override
			public boolean isAdminSession(HttpServletRequest request) {
				return Boolean.TRUE
						.equals(request.getSession().getAttribute("admin"));
			}

			@Override
			public String getRealm(HttpServletRequest request) {
				return REALM;
			}
		});
		
		try {
			config.setConsumerStore(new RdfConsumerStore());
		} catch (ConsumerStoreException e) {
			e.printStackTrace();
		} catch (SQLException e) {
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
			OAuthServlet.handleException(httpResponse, e, REALM);
			return Response.status(Status.UNAUTHORIZED).build();
		}

		// Validation passed. Respond with the secret message.
		return Response.ok("Hello World!").type(MediaType.TEXT_PLAIN).build();
	}
}
