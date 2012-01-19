/*******************************************************************************
 * Copyright (c) 2011, 2012 IBM Corporation.
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

package org.eclipse.lyo.samples.bugzilla;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Properties;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import net.oauth.OAuth;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.OAuthProblemException;
import net.oauth.server.OAuthServlet;

import org.eclipse.lyo.samples.bugzilla.exception.BugzillaOAuthException;
import org.eclipse.lyo.samples.bugzilla.exception.UnauthroziedException;
import org.eclipse.lyo.samples.bugzilla.utils.HttpUtils;
import org.eclipse.lyo.server.oauth.core.Authentication;
import org.eclipse.lyo.server.oauth.core.AuthenticationException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.OAuthRequest;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;
import org.eclipse.lyo.server.oauth.core.token.LRUCache;
import org.eclipse.lyo.server.oauth.core.token.SimpleTokenStrategy;

import com.j2bugzilla.base.BugzillaConnector;
import com.j2bugzilla.base.BugzillaException;
import com.j2bugzilla.base.ConnectionException;
import com.j2bugzilla.rpc.LogIn;

public class BugzillaInitializer implements ServletContextListener {

    private static final String CONNECTOR_ATTRIBUTE = "org.eclipse.lyo.samples.bugzilla.BugzillaConnector";
	private static String baseUri = null;
    private static String bugzillaUri = null;
	private static boolean provideHtml = true;
	
	/*
	 * We can't rely on session tracking always working for OAuth requests, so store the BugzillaConnector in a map
	 * with the OAuth token as the key.
	 */
	private static LRUCache<String, BugzillaConnector> keyToConnectorCache = new LRUCache<String, BugzillaConnector>(
			200);
	
	@Override
	public void contextInitialized(ServletContextEvent event) {
		OAuthConfiguration config = OAuthConfiguration.getInstance();

		// The realm used in 401 unauthorized responses.
		config.setRealm("Bugzilla");

		// Validates a user's ID and password.
		config.setAuthentication(new Authentication() {
			@Override
			public void login(HttpServletRequest request, String id,
					String password) throws AuthenticationException {
				try {
					BugzillaConnector bc = new BugzillaConnector();
					bc.connectTo(bugzillaUri + "/xmlrpc.cgi");
					LogIn login = new LogIn(id, password);
					bc.executeMethod(login);
					request.setAttribute(CONNECTOR_ATTRIBUTE, bc);
				} catch (Exception e) {
					throw new AuthenticationException(e.getCause().getMessage(), e);
				}
			}

			@Override
			public String getApplicationName() {
				// Display name for this application.
				return "Bugzilla";
			}
		});

		/*
		 * Override some SimpleTokenStrategy methods so that we can keep the
		 * BugzillaConnection associated with the OAuth tokens.
		 */
		config.setTokenStrategy(new SimpleTokenStrategy() {
			@Override
			public void markRequestTokenAuthorized(
					HttpServletRequest httpRequest, String requestToken)
					throws OAuthProblemException {
				keyToConnectorCache.put(requestToken,
						(BugzillaConnector) httpRequest.getAttribute(CONNECTOR_ATTRIBUTE));
				super.markRequestTokenAuthorized(httpRequest, requestToken);
			}

			@Override
			public void generateAccessToken(OAuthRequest oAuthRequest)
					throws OAuthProblemException, IOException {
				String requestToken = oAuthRequest.getMessage().getToken();
				BugzillaConnector bc = keyToConnectorCache.remove(requestToken);
				super.generateAccessToken(oAuthRequest);
				keyToConnectorCache.put(oAuthRequest.getAccessor().accessToken, bc);
			}
		});
		
		try {
			// For now, hard-code the consumers.
			config.setConsumerStore(new ConsumerStore() {
				@Override
				public Collection<LyoOAuthConsumer> load() throws IOException {
					// Define one consumer with key "key" and secret "secret".
					LyoOAuthConsumer consumer = new LyoOAuthConsumer("rtc", "sesame");
					consumer.setName("Rational Team Concert");
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
	
    static {
        Properties props = new Properties();
        try {
            props.load(BugzillaInitializer.class.getResourceAsStream("/bugz.properties"));
            baseUri     = props.getProperty("adapter_uri");
            bugzillaUri = props.getProperty("bugzilla_uri");
            if (props.getProperty("provideHtml") != null) {
                provideHtml = Boolean.parseBoolean(props.getProperty("provideHtml"));
            }
            System.out.println("adapter_uri: "  + baseUri);
            System.out.println("bugzilla_uri: " + bugzillaUri);
            System.out.println("provideHtml: "  + provideHtml);
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

	public static BugzillaConnector getBugzillaConnector(Credentials credentials)
			throws ConnectionException, UnauthroziedException {
		BugzillaConnector bc = new BugzillaConnector();
		bc.connectTo(bugzillaUri + "/xmlrpc.cgi");
		LogIn login = new LogIn(credentials.getUsername(), credentials.getPassword());
		try {
			bc.executeMethod(login);
		} catch (BugzillaException e) {
			throw new UnauthroziedException(e.getCause().getMessage());
		}
		return bc;
	}

	public static BugzillaConnector getBugzillaConnector(
			HttpServletRequest request) throws ConnectionException,
			UnauthroziedException, IOException, ServletException {
		// First check if this is an OAuth request.
		try {
			OAuthMessage message = OAuthServlet.getMessage(request, null);
			if (message.getToken() != null) {
				OAuthRequest oAuthRequest = new OAuthRequest(request);
				oAuthRequest.validate();
				BugzillaConnector connector = keyToConnectorCache.get(message
						.getToken());
				if (connector == null) {
					throw new OAuthProblemException(
							OAuth.Problems.TOKEN_EXPIRED);
				}

				return connector;
			}
		} catch (OAuthException e) {
			throw new BugzillaOAuthException(e);
		}
		
		// This is not an OAuth request. Check for basic access authentication.
		HttpSession session = request.getSession();
		BugzillaConnector connector = (BugzillaConnector) session
				.getAttribute(CONNECTOR_ATTRIBUTE);
		if (connector == null) {
			Credentials credentials = HttpUtils.getCredentials(request);
			if (credentials == null) {
				throw new UnauthroziedException();
			}
			connector = getBugzillaConnector(credentials);
			session.setAttribute(CONNECTOR_ATTRIBUTE, connector);
		}
		
		return connector;
	}

	public static String getBaseUri() {
        return baseUri;
    }

    public static void setBaseUri(String baseUri) {
        BugzillaInitializer.baseUri = baseUri;
    }

    public static String getBugzillaUri() {
        return bugzillaUri;
    }

    public static void setBugzillaUri(String bugzillaUri) {
        BugzillaInitializer.bugzillaUri = bugzillaUri;
    }

    public static boolean isProvideHtml() {
        return provideHtml;
    }

    public static void setProvideHtml(boolean aProvideHtml) {
        provideHtml = aProvideHtml;
    }
}
