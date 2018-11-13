/*******************************************************************************
 * Copyright (c) 2012, 2014 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Michael Fiedler     - initial API and implementation for Bugzilla adapter
 *     Susumu Fukuda       - extracted this class from Bugzilla adapter CredentialsFilter
 *     Samuel Padgett      - fix NPEx when Exception.getCause() returns null in Application.login()
 *******************************************************************************/
package org.eclipse.lyo.server.oauth.core.utils;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import net.oauth.OAuth;
import net.oauth.OAuthAccessor;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.OAuthProblemException;
import net.oauth.http.HttpMessage;
import net.oauth.server.OAuthServlet;

import org.eclipse.lyo.server.oauth.core.Application;
import org.eclipse.lyo.server.oauth.core.AuthenticationException;
import org.eclipse.lyo.server.oauth.core.OAuthConfiguration;
import org.eclipse.lyo.server.oauth.core.OAuthRequest;
import org.eclipse.lyo.server.oauth.core.consumer.ConsumerStore;
import org.eclipse.lyo.server.oauth.core.consumer.LyoOAuthConsumer;
import org.eclipse.lyo.server.oauth.core.token.LRUCache;
import org.eclipse.lyo.server.oauth.core.token.SimpleTokenStrategy;

/**
 * <h3>Overview</h3>
 * Purpose: Provide a JEE Servlet filter base implementation for accepting
 * both HTTP basic and OAuth provider authentication, connecting your tool using the 
 * credentials, and managing the connections.
 * 
 * <p>With this credentitals filter:<ul>
 * <li>Your Webapp can accepts HTTP Basic authentication
 * <li>Your Webapp can works as an OAuth provider
 * </ul>
 * <p>Once user entered credentials via HTTP Basic auth or OAuth, it
 * is passed to a callback method {@link #getCredentialsFromRequest(HttpServletRequest)}
 * or {@link #getCredentialsForOAuth(String, String)} so that your implementation
 * can build a Credentials object from the given data.
 * And then, next callback method {@link #login(Object, HttpServletRequest)} is invoked for
 * authenticate the credentials and building connection to your back-end tool.
 * Concrete types of the credentials and the connection can be specified as type
 * parameters of this class.
 * 
 * <p>While processing a request, the credentials and the connection are available
 * as attributes of the request. Your subsequent process such as {@link HttpServlet#service(ServletRequest, ServletResponse)}
 * can extract and use them for accessing your tool. You can use {@link #getConnector(HttpServletRequest)}
 * and {@link #getCredentials(HttpServletRequest)} to retrieve them from the request.
 * 
 * <h3>Usage</h3>
 * <p>You have to subclass this class and give implementations for the following methods:
 * <ul>
 *  <li>{@link #login(Object, HttpServletRequest)}
 *  <li>{@link #getCredentialsFromRequest(HttpServletRequest)}
 *  <li>{@link #getCredentialsForOAuth(String, String)}
 *  <li>{@link #isAdminSession(String, Object, HttpServletRequest)}
 *  <li>{@link #createConsumerStore()}
 *  <li>{@link #logout(Object, HttpSession)} (optional)
 * </ul>
 * Then, add the follwoing filter-mapping to your web.xml:
 * <pre>
 *   &lt;filter&gt;
 *    &lt;display-name&gt;[YOUR FILTER CLASS NAME (MyFilter)]&lt;/display-name&gt;
 *    &lt;filter-name&gt;[YOUR FILTER CLASS NAME (MyFilter)]&lt;/filter-name&gt;
 *    &lt;filter-class&gt;[FULLY QUALIFIED YOUR FILTER CLASS NAME (com.example.MyFilter)]&lt;/filter-class&gt;
 *  &lt;/filter&gt;
 *  &lt;filter-mapping&gt;
 *    &lt;filter-name&gt;[YOUR FILTER CLASS NAME (MyFilter)]&lt;/filter-name&gt;
 *    &lt;url-pattern&gt;/services/*&lt;/url-pattern&gt;
 *  &lt;/filter-mapping&gt;
 * </pre>
 * 
 * @param <Connection> Type for connection object to your tool
 * @param <Credentials> Type for credentials for your tool. (e.g. UsernamePasswordCredentials)
 */
abstract public class AbstractAdapterCredentialsFilter<Credentials, Connection> implements Filter {
	
	private static final String ATTRIBUTE_BASE = "org.eclipse.lyo.server.oauth.core.utils.";
	public static final String CONNECTOR_ATTRIBUTE = ATTRIBUTE_BASE + "Connector";
	public static final String CREDENTIALS_ATTRIBUTE = ATTRIBUTE_BASE + "Credentials";
	public static final String ADMIN_SESSION_ATTRIBUTE = ATTRIBUTE_BASE + "AdminSession";
	public static final String JAZZ_INVALID_EXPIRED_TOKEN_OAUTH_PROBLEM = "invalid_expired_token";
	public static final String OAUTH_EMPTY_TOKEN_KEY = new String("OAUTH_EMPTY_TOKEN_KEY");
    
    private final LRUCache<String, Connection> tokenToConnectionCache = new LRUCache<String, Connection>(200);
    
    final private String displayName;
    final private String realm;
    
    /**
     * Constructor
     * @param displayName application name displayed on the login prompt
     * @param realm realm for this adapter
     */
    protected AbstractAdapterCredentialsFilter(String displayName, String realm) {
    	this.displayName = displayName;
    	this.realm = realm;
    }
    
    /**
     * Extract credentials from the request and return it.
     * @param request {@link HttpServletRequest}
     * @return credentials
     * @throws UnauthorizedException iff no login credentials associated to the request.
     */
    abstract protected Credentials getCredentialsFromRequest(HttpServletRequest request) throws UnauthorizedException;

    /**
     * Create a Credentials object from given user id and password.
     * 
     * <p>For OAuth two-legged request, the <code>id</code> is set to {@link #OAUTH_EMPTY_TOKEN_KEY}
     * object. Implementor can compare the value using <code>==</code> to identify the request.
     * In the request the consumer key is set to the <code>password</code>. So you might find a functional
     * user associated to the consumer key with the value.
     * @param id user id or {@link #OAUTH_EMPTY_TOKEN_KEY}
     * @param password password or OAuth consumer key
     * @return credentials
     */
	abstract protected Credentials getCredentialsForOAuth(String id, String password);

	/**
     * Create connection to your tool using the given credentials, and returns the connection.
     * @param crdentials credentials for login
     * @param request {@link HttpServletRequest}
     * @return connection that represents the successful login session
     * @throws UnauthorizedException credentials is invalid
     * @throws ServletException other exceptional situation
     */
    abstract protected Connection login(Credentials crdentials, HttpServletRequest request) throws UnauthorizedException, ServletException;
    
    /**
     * Logout
     * @param loginSession
     * @param session
     */
    protected void logout(Connection loginSession, HttpSession session) {
    	// do nothing by default
    }
    
    /**
     * Tell if this is an admin session. For admin session, Lyo provides user-interface to
     * accept provisional authentication key.
     * @param id
     * @param session
     * @param request
     * @return
     */
    abstract protected boolean isAdminSession(String id, Connection session, HttpServletRequest request);
    
    /**
     * Invoked from this class to create {@link ConsumerStore} for OAuth keys.
     * Typical implementation can be:
     * <pre>return new FileSystemConsumerStore("YourOAuthStore.xml");
     * </pre>
     * @return
     * @throws Exception
     */
    abstract protected ConsumerStore createConsumerStore() throws Exception;
    
    /**
     * get Connector assigned to this request
     * 
     * The connector should be placed in the session by the CredentialsFilter servlet filter
     * 
     * @param request
     * @return connector 
     */
	public static <T> T getConnector(HttpServletRequest request) 
	{	
		//connector should never be null if CredentialsFilter is doing its job
		@SuppressWarnings("unchecked")
		T connector = (T) request.getAttribute(CONNECTOR_ATTRIBUTE);	
		return connector;
	}
	
	/**
	 * Get Credentials for this session 
	 * @param request
	 * @return credentials
	 */
	public static <T> T getCredentials(HttpServletRequest request)
	{
		@SuppressWarnings("unchecked")
		T credentials = (T) request.getSession().getAttribute(CREDENTIALS_ATTRIBUTE);
		return credentials;
	}
	
    protected String getOAuthRealm() {
    	return realm;
    }
	protected String getDisplayName() {
		return displayName;
	}
	

    @Override
    public void destroy() {
    	
    }
		
	/**
	 * Check for OAuth or BasicAuth credentials and challenge if not found.
	 * 
	 * Store the Connector in the HttpSession for retrieval in the REST services.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse,
			FilterChain chain) throws IOException, ServletException {
		
		if(servletRequest instanceof HttpServletRequest && servletResponse instanceof HttpServletResponse) {
			HttpServletRequest request = (HttpServletRequest) servletRequest;
			HttpServletResponse response = (HttpServletResponse) servletResponse;
		
			boolean isTwoLeggedOAuthRequest = false;
			String twoLeggedOAuthConsumerKey = null;
			
			//Don't protect requests to oauth service.   TODO: possibly do this in web.xml
			if (! request.getPathInfo().startsWith("/oauth"))
			{
			
				// First check if this is an OAuth request.
				try {
					try {
						OAuthMessage message = OAuthServlet.getMessage(request, null);
						// test if this is a valid two-legged oauth request
						if ("".equals(message.getToken())) {
							validateTwoLeggedOAuthMessage(message);
							isTwoLeggedOAuthRequest = true;
							twoLeggedOAuthConsumerKey = message.getConsumerKey();
						}
						
						if (!isTwoLeggedOAuthRequest && message.getToken() != null) {
							OAuthRequest oAuthRequest = new OAuthRequest(request);
							oAuthRequest.validate();
							Connection connector = tokenToConnectionCache.get(message.getToken());
							if (connector == null) {
								throw new OAuthProblemException(
										OAuth.Problems.TOKEN_REJECTED);
							}
			
							request.getSession().setAttribute(CONNECTOR_ATTRIBUTE, connector);
						}
					} catch (OAuthProblemException e) {
						if (OAuth.Problems.TOKEN_REJECTED.equals(e.getProblem()))
							throwInvalidExpiredException(e);
						else
							throw e;
					}
				} catch (OAuthException e) {
					OAuthServlet.handleException(response, e, getOAuthRealm());
					return;
				} catch (URISyntaxException e) {
					throw new ServletException(e);
				}
				
				// Check for Basic authentication if this is not an OAuth request
				HttpSession session = request.getSession();
				Connection connector = (Connection) session.getAttribute(CONNECTOR_ATTRIBUTE);
				if (connector == null) {
					try {
						Credentials credentials;
						if (isTwoLeggedOAuthRequest) {
							connector = tokenToConnectionCache.get("");
							if (connector == null) {
								credentials = getCredentialsForOAuth(OAUTH_EMPTY_TOKEN_KEY, twoLeggedOAuthConsumerKey);
								connector = login(credentials, request);
								tokenToConnectionCache.put("", connector);
							}
							credentials = null; // TODO; Do we need to keep the credentials for this path ??
						} else {
							credentials = (Credentials) request.getSession().getAttribute(CREDENTIALS_ATTRIBUTE);
							if (credentials == null)
							{
								credentials = getCredentialsFromRequest(request);
								if (credentials == null) {
									throw new UnauthorizedException();
								}
							}
							connector = login(credentials, request);
						}
						session.setAttribute(CONNECTOR_ATTRIBUTE, connector);
						session.setAttribute(CREDENTIALS_ATTRIBUTE, credentials);
				
					} catch (UnauthorizedException e)
					{
						sendUnauthorizedResponse(response, e);
						System.err.println(e.getMessage());
						return;
					} catch (ServletException ce)
					{
						throw ce;
					}
				}
				
				if (connector != null) {
					doChainDoFilterWithConnector(request, response, chain, connector);
					return;
				}
				
			}
		}
		
		chain.doFilter(servletRequest, servletResponse);
	}
	
	/**
	 * The default implementation is:
	 * <pre>
	 * request.setAttribute(CONNECTOR_ATTRIBUTE, connector);
	 * chain.doFilter(request, response);</pre>
	 * 
	 * Subclass may invoke the <code>chain.doFilter()</code> directly instead of invoking super method.
	 * 
	 * @param request {@link HttpServletRequest}
	 * @param response {@link HttpServletResponse}
	 * @param chain {@link FilterChain}
	 * @param sessionConnector {@link Connector} to be used for processing rest of the chain (i.e. REST request)
	 * @throws IOException
	 * @throws ServletException
	 */
	protected void doChainDoFilterWithConnector(HttpServletRequest request,
			HttpServletResponse response, FilterChain chain, Connection connector) throws IOException, ServletException {
		request.setAttribute(CONNECTOR_ATTRIBUTE, connector);
		chain.doFilter(request, response);
	}

	private void validateTwoLeggedOAuthMessage(OAuthMessage message)
			throws IOException, OAuthException,
			URISyntaxException {
		OAuthConfiguration config = OAuthConfiguration.getInstance();
		LyoOAuthConsumer consumer = config.getConsumerStore().getConsumer(message.getConsumerKey());
		if (consumer != null && consumer.isTrusted()) {
			// The request can be a two-legged oauth request because it's a trusted consumer
			// Validate the message with an empty token and an empty secret
			OAuthAccessor accessor = new OAuthAccessor(consumer);
			accessor.requestToken = "";
			accessor.tokenSecret = "";
			config.getValidator().validateMessage(message, accessor);
		} else {
			throw new OAuthProblemException(
					OAuth.Problems.TOKEN_REJECTED);
		}
	}
	
	private HttpSessionListener listener = new HttpSessionListener() {
		@Override
		public void sessionDestroyed(HttpSessionEvent se) {
			HttpSession session = se.getSession();
			@SuppressWarnings("unchecked")
			Connection loginSession = (Connection) session.getAttribute(CONNECTOR_ATTRIBUTE);
			logout(loginSession, session);
		}
		
		@Override
		public void sessionCreated(HttpSessionEvent se) {
			// nothing
		}
	};
	
	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
		OAuthConfiguration config = OAuthConfiguration.getInstance();
		
		// Add session listener
		filterConfig.getServletContext().addListener(listener);

		// Validates a user's ID and password.
		config.setApplication(new Application() {
			@Override
			public void login(HttpServletRequest request, String id,
					String password) throws AuthenticationException {
				try {
					Credentials creds = getCredentialsForOAuth(id, password);
					request.getSession().setAttribute(CREDENTIALS_ATTRIBUTE, creds);
					
					Connection c = AbstractAdapterCredentialsFilter.this.login(creds, request);
					request.setAttribute(CONNECTOR_ATTRIBUTE, c);
					
					boolean isAdmin = AbstractAdapterCredentialsFilter.this.isAdminSession(id, c, request);
					request.getSession().setAttribute(ADMIN_SESSION_ATTRIBUTE, isAdmin);
				} catch (Exception e) {
					if (e.getCause() != null) {
						throw new AuthenticationException(e.getCause().getMessage(), e);
					} else {
						throw new AuthenticationException(e);
					}
				}
			}

			@Override
			public String getName() {
				// Display name for this application.
				return getDisplayName();
			}

			@Override
			public boolean isAdminSession(HttpServletRequest request) {
				return Boolean.TRUE.equals(request.getSession().getAttribute(
						ADMIN_SESSION_ATTRIBUTE));
			}

			@Override
			public String getRealm(HttpServletRequest request) {
				return getOAuthRealm();
			}

			@Override
			public boolean isAuthenticated(HttpServletRequest request) {
				@SuppressWarnings("unchecked")
				Connection connector = (Connection) request.getSession().getAttribute(CONNECTOR_ATTRIBUTE);
				if (connector == null) {
					return false;
				}
				request.setAttribute(CONNECTOR_ATTRIBUTE, connector);
				return true;
			}
		});

		/*
		 * Override some SimpleTokenStrategy methods so that we can keep the
		 * Connector associated with the OAuth tokens.
		 */
		config.setTokenStrategy(new SimpleTokenStrategy() {
			@SuppressWarnings("unchecked")
			@Override
			public void markRequestTokenAuthorized(
					HttpServletRequest httpRequest, String requestToken)
					throws OAuthProblemException {
				tokenToConnectionCache.put(requestToken,
						(Connection) httpRequest.getAttribute(CONNECTOR_ATTRIBUTE));
				super.markRequestTokenAuthorized(httpRequest, requestToken);
			}

			@Override
			public void generateAccessToken(OAuthRequest oAuthRequest)
					throws OAuthProblemException, IOException {
				String requestToken = oAuthRequest.getMessage().getToken();
				Connection bc = tokenToConnectionCache.remove(requestToken);
				super.generateAccessToken(oAuthRequest);
				tokenToConnectionCache.put(oAuthRequest.getAccessor().accessToken, bc);
			}
		});

		try {
			// For now, hard-code the consumers.
			config.setConsumerStore(createConsumerStore());
		} catch (Throwable t) {
			System.err.println("Error initializing the OAuth consumer store: " +  t.getMessage());
		}
	}
	
	/**
	 * Jazz requires a exception with the magic string "invalid_expired_token" to restart
	 * OAuth authentication
	 * @param e
	 * @return
	 * @throws OAuthProblemException 
	 */
	private void throwInvalidExpiredException(OAuthProblemException e) throws OAuthProblemException {
		OAuthProblemException ope = new OAuthProblemException(JAZZ_INVALID_EXPIRED_TOKEN_OAUTH_PROBLEM);
		ope.setParameter(HttpMessage.STATUS_CODE, new Integer(
				HttpServletResponse.SC_UNAUTHORIZED));
		throw ope;
	}
	
	private void sendUnauthorizedResponse(HttpServletResponse response,
			UnauthorizedException e) throws IOException, ServletException {
		// Accept basic access or OAuth authentication.
		final String WWW_AUTHENTICATE_HEADER = "WWW-Authenticate";
		final String BASIC_AUTHORIZATION_PREFIX = "Basic ";
		final String BASIC_AUTHENTICATION_CHALLENGE = BASIC_AUTHORIZATION_PREFIX
				+ "realm=\"" + getOAuthRealm() + "\"";
		final String OAUTH_AUTHORIZATION_PREFIX = "OAuth ";
		final String OAUTH_AUTHENTICATION_CHALLENGE = OAUTH_AUTHORIZATION_PREFIX
				+ "realm=\"" + getOAuthRealm() + "\"";

		response.addHeader(WWW_AUTHENTICATE_HEADER,
				OAUTH_AUTHENTICATION_CHALLENGE);
		response.addHeader(WWW_AUTHENTICATE_HEADER,
				BASIC_AUTHENTICATION_CHALLENGE);
		response.sendError(HttpServletResponse.SC_UNAUTHORIZED);
	}

}
