/*******************************************************************************
 * Copyright (c) 2011, 2016 IBM Corporation.
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
 *	 Michael Fiedler	 - initial API and implementation
 *	 Michael Fiedler	 - refactoring to remove un-needed GETs in formLogin()
 *	 Samuel Padgett 	 - improve error handling
 *   Jim Ruehin			 - added Basic auth for Jazz Auth Server 6.x
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.jazz;

import java.io.IOException;
import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.util.EntityUtils;
import org.apache.xerces.impl.dv.util.Base64;
import org.eclipse.lyo.client.exception.exception.JazzAuthErrorException;
import org.eclipse.lyo.client.exception.exception.JazzAuthFailedException;
import org.eclipse.lyo.client.oslc.OslcClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An OSLC client for IBM Rational Jazz servers using Form Auth to authenticate.
 * Accesses the Jazz rootservices URL to lookup the OSLC Catlalog location
 *
 * This class is not currently thread safe.
 *
 */
public class JazzFormAuthClient extends OslcClient {

	private final static Logger logger = LoggerFactory.getLogger(JazzFormAuthClient.class);
	private String url;
	private String authUrl;
	private String project;
	private String user;
	private String password;
	private HttpResponse lastRedirectResponse = null;
	private String jsaCsrfCookie = null;

	private static final String JAZZ_AUTH_MESSAGE_HEADER = "X-com-ibm-team-repository-web-auth-msg";
	private static final String JAZZ_AUTH_FAILED = "authfailed";
	private static final String WWW_AUTHENTICATE_HEADER = "WWW-Authenticate";
	private static final String JAZZ_JSA_REDIRECT_HEADER = "X-JSA-AUTHORIZATION-REDIRECT";

	public JazzFormAuthClient()
	{
		super();
	}

	/**
	 * Create a new Jazz Form Auth client for the given URL, user and password
	 *
	 * @param url - the URL of the Jazz server, including the web app context
	 * @param user
	 * @param password
	 **/
	public JazzFormAuthClient(String url, String user, String password)
	{
		this();
		this.url=url;
		this.authUrl = url;  //default to base URL
		this.user = user;
		this.password = password;

	}

	/**
	 * Create a new Jazz Form Auth client for the given URL, user and password
	 *
	 * @param url - the URL of the Jazz server, including the web app context
	 * @param authUrl - the base URL to use for authentication.  This is normally the
	 * application base URL for RQM and RTC and is the JTS application URL for fronting
	 * applications like RRC and DM.
	 * @param user
	 * @param password
	 **/
	public JazzFormAuthClient(String url, String authUrl, String user, String password)
	{
		this(url, user, password);
		this.authUrl = authUrl;
	}

	public String getUrl() {
		return url;
	}
	public void setUrl(String url) {
		this.url = url;
	}

	public String getAuthUrl() {
		return authUrl;
	}

	public void setAuthUrl(String authUrl) {
		this.authUrl = authUrl;
	}

	public String getProject() {
		return project;
	}
	public void setProject(String project) {
		this.project = project;
	}
	public String getUser() {
		return user;
	}
	public void setUser(String user) {
		this.user = user;
	}
	public String getPassword() {
		return password;
	}
	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 * Executes the sequence of HTTP requests to perform a form login to a Jazz server
	 *
	 * @throws JazzAuthFailedException
	 * @throws JazzAuthErrorException
	 * @throws IOException
	 * @throws ClientProtocolException
	 *
	 * @return The HTTP status code of the final request to verify login is successful
	 **/
	public int login()
			throws JazzAuthFailedException, JazzAuthErrorException, ClientProtocolException, IOException {

		int statusCode = -1;
		String location = null;
		HttpResponse resp;
		
		HttpGet authenticatedIdentity = new HttpGet(this.authUrl + "/authenticated/identity");
		resp = httpClient.execute(authenticatedIdentity);
		statusCode = resp.getStatusLine().getStatusCode();
		location = getHeader(resp,"Location");
		EntityUtils.consume(resp.getEntity());
		statusCode = followRedirects(statusCode,location);

		// Check to see if the response is from a Jazz Authorization Server that supports OIDC.
		// In CLM 6.x, the JAS supports Basic auth to be compatible with earlier releases.
		// If we're talking to a JAS that supports OIDC, re-do the request with a Basic auth header to gain access.
		if (HttpStatus.SC_UNAUTHORIZED == statusCode) { // this might be a JAS server. 
			if (true == handleJsaServer()) {
				// Re-do the original request using Basic auth, starting at the last authorization redirect.
				authenticatedIdentity = new HttpGet(lastRedirectResponse.getFirstHeader(JAZZ_JSA_REDIRECT_HEADER).getValue() + "&prompt=none");
				String credentials = new String(user + ":" + password);
				authenticatedIdentity.setHeader("Authorization", "Basic " + Base64.encode(credentials.getBytes("UTF-8")));
				resp = httpClient.execute(authenticatedIdentity);
				statusCode = resp.getStatusLine().getStatusCode();
				EntityUtils.consume(resp.getEntity());		
				statusCode = followRedirects(statusCode, getHeader(resp,"Location"));
				return statusCode;
			}
		}
		
		
		HttpPost securityCheck = new HttpPost(this.authUrl + "/j_security_check");
		StringEntity entity = new StringEntity("j_username=" + this.user + "&j_password=" + this.password);
		securityCheck.setHeader("Accept", "*/*");
		securityCheck.setHeader("X-Requested-With", "XMLHttpRequest");
		securityCheck.setEntity(entity);
		securityCheck.addHeader("Content-Type", "application/x-www-form-urlencoded; charset=utf-8");
		securityCheck.addHeader("OSLC-Core-Version", "2.0");
		securityCheck.addHeader("Cookie", jsaCsrfCookie);

		resp = httpClient.execute(securityCheck);
		statusCode = resp.getStatusLine().getStatusCode();

		String jazzAuthMessage = null;
		Header jazzAuthMessageHeader = resp.getLastHeader(JAZZ_AUTH_MESSAGE_HEADER);
		if (jazzAuthMessageHeader != null) {
			jazzAuthMessage = jazzAuthMessageHeader.getValue();
		}

		if (jazzAuthMessage != null && jazzAuthMessage.equalsIgnoreCase(JAZZ_AUTH_FAILED))
		{
			EntityUtils.consume(resp.getEntity());
			throw new JazzAuthFailedException(this.user,this.url);
		}
		else if ( statusCode != HttpStatus.SC_OK && statusCode != HttpStatus.SC_MOVED_TEMPORARILY )
		{
			EntityUtils.consume(resp.getEntity());
			throw new JazzAuthErrorException(statusCode, this.url);
		}
		else //success
		{
			location = getHeader(resp,"Location");
			EntityUtils.consume(resp.getEntity());
			statusCode = followRedirects(statusCode,location);
		}

		return statusCode;
	}
	
	/**
	 * Checks to see if we're communicating with a JSA server (SSO), and handles auth for JSA
	 * This function is called because the server returned a 401 (UNAUTHORIZED).
	 * If this is not a JSA server, we return to the normal flow. IF it is a server, we get the 
	 * 
	 * @return true if it's a JSA server, else false.
	 * 
	 * @throws IOException 
	 * @throws ClientProtocolException 
	 * @throws JazzAuthErrorException 

	 */
	private Boolean handleJsaServer() throws ClientProtocolException, IOException, JazzAuthErrorException
	{
		if (null == lastRedirectResponse) {
			return false;
		}

		// If this is a JAS response supporting OIDC, then we expect both Basic and Bearer challenges
		Header[] authHeaders = lastRedirectResponse.getHeaders(WWW_AUTHENTICATE_HEADER);
		
		if (2 > authHeaders.length) { // if we don't have at least 2 auth headers then it's not JSA
			return false; //throw new JazzAuthFailedException(this.user,this.url);
		}
		
		Boolean basicChallenge=false, 
				bearerChallenge=false;
		for (Header theHeader : authHeaders) {
			if (theHeader.getValue().contains("Basic")) {
				basicChallenge = true;
			}
			else if (theHeader.getValue().contains("Bearer")) {
				bearerChallenge = true;
			}
		}
		if (!basicChallenge || !bearerChallenge) { // didn't get both challenges, so this isn't a JSA server
			return false; //throw new JazzAuthFailedException(this.user,this.url);
		}
		
		// Check for the JSA authoriziation redirect header. If we don't have it, it's not a JSA server.
		Header jsaRedirectHeader = lastRedirectResponse.getFirstHeader(JAZZ_JSA_REDIRECT_HEADER);
		if (null == jsaRedirectHeader) {
			return false; 
		}

		// Passed all checks - we're interacting with a JAS.
		return true;
	}


	/**
	 * Executes the sequence of HTTP requests to perform a form login to a Jazz server
	 * @throws JazzAuthFailedException
	 * @throws JazzAuthErrorException
	 *
	 * @return The HTTP status code of the final request to verify login is successful
	 *
	 * @deprecated Use {@link #login()}.
	 */
	@Deprecated
	public  int formLogin() throws JazzAuthFailedException, JazzAuthErrorException {
		try
		{
			return login();
		} catch (JazzAuthFailedException jfe) {
			throw jfe;
		} catch (JazzAuthErrorException jee) {
			throw jee;
		}catch (Exception e) {
			logger.error("Form login failed for unknown reason", e);
			return -1;
		}
	}


	private int followRedirects(int statusCode, String location) throws ClientProtocolException, IOException
	{
		while ( ((statusCode == HttpStatus.SC_MOVED_TEMPORARILY) || (HttpStatus.SC_SEE_OTHER == statusCode)) && (location != null))
		{
			HttpGet get = new HttpGet(location);
			
			lastRedirectResponse = this.httpClient.execute(get);
			statusCode = lastRedirectResponse.getStatusLine().getStatusCode();
			location = getHeader(lastRedirectResponse,"Location");
			EntityUtils.consume(lastRedirectResponse.getEntity());
		}

		return statusCode;
	}

	private String getHeader(HttpResponse resp, String headerName)
	{
		String retval = null;
		Header header =  resp.getFirstHeader(headerName);
		if (header != null)
			retval = header.getValue();
		return retval;
	}
}
