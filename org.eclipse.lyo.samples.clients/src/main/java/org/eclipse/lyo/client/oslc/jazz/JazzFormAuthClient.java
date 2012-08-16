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
 *     Michael Fiedler     - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.jazz;

import java.io.IOException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.EntityUtils;
import org.eclipse.lyo.client.oslc.OslcClient;

public class JazzFormAuthClient extends OslcClient {
	
	private String url;
	private String project;
	private String user;
	private String password;

	
	public JazzFormAuthClient()
	{
		super(); 
	}
	
	public JazzFormAuthClient(String url, String user, String password) 
	{
		this();
		this.url=url;
		this.user = user;
		this.password = password;
		
	}
	
	public String getUrl() {
		return url;
	}
	public void setUrl(String url) {
		this.url = url;
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
	
	public  int formLogin() {

		int statusCode = -1;
		String location = null;
		
		HttpResponse resp;
		try 
		{
			HttpGet get1 = new HttpGet(this.url + "/auth/authrequired");
			resp = httpClient.execute(get1);
			statusCode = resp.getStatusLine().getStatusCode();
			location = getHeader(resp,"Location");
			EntityUtils.consume(resp.getEntity());
			followRedirects(statusCode,location);
			
			
			HttpGet get2= new HttpGet(this.url + "/authenticated/identity");
			
			resp = httpClient.execute(get2);
			statusCode = resp.getStatusLine().getStatusCode();
			location = getHeader(resp,"Location");
			EntityUtils.consume(resp.getEntity());
			followRedirects(statusCode,location);
			
			
			HttpPost post = new HttpPost(this.url + "/j_security_check");
			StringEntity entity = new StringEntity("j_username=" + this.user + "&j_password=" + this.password);
			post.setHeader("Accept", "*/*");
			post.setEntity(entity);
			post.addHeader("Content-Type", "application/x-www-form-urlencoded; charset=utf-8");
		    post.addHeader("OSLC-Core-Version", "2.0");
		    
		    resp = httpClient.execute(post);
		    statusCode = resp.getStatusLine().getStatusCode();
		    if ( statusCode != HttpStatus.SC_OK && statusCode != HttpStatus.SC_MOVED_TEMPORARILY )
		    {
		    	System.out.println("Login failed with status code: " + statusCode);
		    }
		    else
		    {
		    	location = getHeader(resp,"Location");
		    	EntityUtils.consume(resp.getEntity());
		    	followRedirects(statusCode,location);
		    	HttpGet get3 = new HttpGet(this.url + "/service/com.ibm.team.repository.service.internal.webuiInitializer.IWebUIInitializerRestService/initializationData");
		    	resp = httpClient.execute(get3);
		    	statusCode = resp.getStatusLine().getStatusCode();
		    	location = getHeader(resp,"Location");
		    	EntityUtils.consume(resp.getEntity());
		    	followRedirects(statusCode,location);
		    	
		    }

		} catch (Exception e) {
			e.printStackTrace();
		}
		return statusCode;
	}
	
	private void followRedirects(int statusCode, String location)
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
	}
	
	private String getHeader(HttpResponse resp, String headerName)
	{
		String retval = null;
		Header header =  resp.getFirstHeader(headerName);
		if (header != null)
			retval = header.getValue();
		return retval;
	}
	
	
	public HttpResponse getArtifactFeed(String feedUrl)
	{
		HttpResponse resp = null;
		try {
			HttpGet feedGet = new HttpGet(feedUrl);
			feedGet.addHeader("Accept", "application/atom+xml");
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
