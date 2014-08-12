/*******************************************************************************
 * Copyright (c) 2011, 2013 IBM Corporation.
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
 *     Michael Fiedler     - initial API and implementation
 *     Michael Fiedler     - refactoring to remove un-needed GETs in formLogin()
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
import org.apache.http.params.HttpParams;
import org.apache.http.util.EntityUtils;
import org.eclipse.lyo.client.exception.JazzAuthErrorException;
import org.eclipse.lyo.client.exception.JazzAuthFailedException;
import org.eclipse.lyo.client.oslc.OSLCConstants;
import org.eclipse.lyo.client.oslc.OslcClient;

/**
 * An OSLC client for IBM Rational Jazz servers using Form Auth to authenticate.
 * Accesses the Jazz rootservices URL to lookup the OSLC Catlalog location
 *
 * This class is not currently thread safe.
 *
 */
public class JazzFormAuthClient extends OslcClient {

	private String url;
	private String authUrl;
	private String project;
	private String user;
	private String password;

	private static final String JAZZ_AUTH_MESSAGE_HEADER = "X-com-ibm-team-repository-web-auth-msg";
	private static final String JAZZ_AUTH_FAILED = "authfailed";

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
	 * @returns
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
	 * @returns
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
	 * Execute the sequence of HTTP requests to perform a form login to a Jazz server
	 * @throws JazzAuthFailedException
	 * @throws JazzAuthErrorException
	 *
	 * @returns The HTTP status code of the final request to verify login is successful
	 **/
	public  int formLogin() throws JazzAuthFailedException, JazzAuthErrorException {

		int statusCode = -1;
		String location = null;

		HttpResponse resp;
		try
		{

			HttpGet authenticatedIdentity = new HttpGet(this.authUrl + "/authenticated/identity");

			resp = httpClient.execute(authenticatedIdentity);
			statusCode = resp.getStatusLine().getStatusCode();
			location = getHeader(resp,"Location");
			EntityUtils.consume(resp.getEntity());
			statusCode = followRedirects(statusCode,location);


			HttpPost securityCheck = new HttpPost(this.authUrl + "/j_security_check");
			StringEntity entity = new StringEntity("j_username=" + this.user + "&j_password=" + this.password);
			securityCheck.setHeader("Accept", "*/*");
			securityCheck.setHeader("X-Requested-With", "XMLHttpRequest");
			securityCheck.setEntity(entity);
			securityCheck.addHeader("Content-Type", "application/x-www-form-urlencoded; charset=utf-8");
			securityCheck.addHeader("OSLC-Core-Version", "2.0");

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
		} catch (JazzAuthFailedException jfe) {
			throw jfe;
	    } catch (JazzAuthErrorException jee) {
	    	throw jee;
	    }catch (Exception e) {
			e.printStackTrace();
		}
		return statusCode;
	}


	private int followRedirects(int statusCode, String location)
	{

		while ((statusCode == HttpStatus.SC_MOVED_TEMPORARILY) && (location != null))
		{
			HttpGet get = new HttpGet(location);
			try {
				HttpResponse newResp = this.httpClient.execute(get);
				statusCode = newResp.getStatusLine().getStatusCode();
				location = getHeader(newResp,"Location");
				EntityUtils.consume(newResp.getEntity());
			} catch (Exception e) {
				e.printStackTrace();
			}

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


	private HttpResponse getArtifactFeed(String feedUrl)
	{
		HttpResponse resp = null;
		try {
			HttpGet feedGet = new HttpGet(feedUrl);
			feedGet.addHeader("Accept", OSLCConstants.ATOM);
			resp = httpClient.execute(feedGet);
			int statusCode = resp.getStatusLine().getStatusCode();
			if (statusCode != HttpStatus.SC_OK)
			   System.out.println("Status code from feed retrieval: " + statusCode);

		} catch (IOException e) {
			e.printStackTrace();
		}

		return resp;
	}


}
