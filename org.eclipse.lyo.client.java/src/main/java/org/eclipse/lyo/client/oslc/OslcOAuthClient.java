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
package org.eclipse.lyo.client.oslc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import javax.ws.rs.HttpMethod;

import net.oauth.OAuthAccessor;
import net.oauth.OAuthConsumer;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.OAuthServiceProvider;
import net.oauth.client.OAuthClient;
import net.oauth.client.httpclient4.HttpClient4;

import org.apache.http.HttpHeaders;
import org.apache.log4j.Logger;
import org.apache.wink.client.ClientConfig;
import org.apache.wink.client.ClientResponse;
import org.apache.wink.client.RestClient;
import org.eclipse.lyo.oslc4j.provider.jena.JenaProvidersRegistry;


public class OslcOAuthClient extends OslcClient {

	private OAuthAccessor accessor;
	private static Logger LOGGER = Logger.getLogger(OslcOAuthClient.class);
	private String oauth_real_name;
	
	/**
	 * Initialize an OAuthClient with the required OAuth URLs
	 * @param requestTokenURL
	 * @param authorizationTokenURL
	 * @param accessTokenURL
	 * @param consumerKey
	 * @param consumerSecret
	 */
	public OslcOAuthClient(String requestTokenURL,
						   String authorizationTokenURL,
						   String accessTokenURL,
						   String consumerKey,
						   String consumerSecret) {
		super();
		OAuthServiceProvider provider = new OAuthServiceProvider(requestTokenURL, authorizationTokenURL, accessTokenURL);
		OAuthConsumer consumer = new OAuthConsumer("",consumerKey,consumerSecret,provider);
		accessor = new OAuthAccessor(consumer);
		
	}
	
	/**
	 * Initialize an OAuthClient with the required OAuth URLs
	 * @param requestTokenURL
	 * @param authorizationTokenURL
	 * @param accessTokenURL
	 * @param consumerKey
	 * @param consumerSecret
	 */
	public OslcOAuthClient(String requestTokenURL,
						   String authorizationTokenURL,
						   String accessTokenURL,
						   String consumerKey,
						   String consumerSecret,
						   String oauthRealmName ) {
		super();
		OAuthServiceProvider provider = new OAuthServiceProvider(requestTokenURL, authorizationTokenURL, accessTokenURL);
		OAuthConsumer consumer = new OAuthConsumer("",consumerKey,consumerSecret,provider);
		accessor = new OAuthAccessor(consumer);
		oauth_real_name = oauthRealmName;
	}
	
	@Override
	/**
	 * Get an OAuth protected OSLC resource
	 * @see OSLCClient#getResponse(final String url, final String mediaType)
	 */
	public ClientResponse getResource(String url, String mediaType) throws IOException, OAuthException, URISyntaxException
	{
		OAuthMessage message = getResourceInternal(url, HttpMethod.GET, false);
		
		String realm = "Jazz";
		// Change if a different name was detected
		if ( oauth_real_name != null ) {
			realm = oauth_real_name;
		}
		String authHeader = message.getAuthorizationHeader(realm);

		ClientConfig config = getClientConfig();

		javax.ws.rs.core.Application app = new javax.ws.rs.core.Application() {
		       public Set<Class<?>> getClasses() {
		           Set<Class<?>> classes = new HashSet<Class<?>>();
		           classes.addAll(JenaProvidersRegistry.getProviders());
		           
		           return classes;
		       }
		};
		config = config.applications(app);

		RestClient restClient = new RestClient(config);
		
		return restClient.resource(url).accept(mediaType).header("Authorization",authHeader).header("OSLC-Core-Version", "2.0").get();
	}
	
	
	public ClientResponse updateResource(final String url, final Object artifact, String mediaType, String acceptType, String ifMatch) throws IOException, OAuthException, URISyntaxException
	{
		
		OAuthMessage message = getResourceInternal(url, HttpMethod.PUT, false);
		
		String realm = "Jazz";
		// Change if a different name was detected
		if ( oauth_real_name != null ) {
			realm = oauth_real_name;
		}
		String authHeader = message.getAuthorizationHeader(realm);

		ClientConfig config = getClientConfig();

		javax.ws.rs.core.Application app = new javax.ws.rs.core.Application() {
		       public Set<Class<?>> getClasses() {
		           Set<Class<?>> classes = new HashSet<Class<?>>();
		           classes.addAll(JenaProvidersRegistry.getProviders());
		           
		           return classes;
		       }
		};
		config = config.applications(app);

		RestClient restClient = new RestClient(config);
		return restClient.resource(url).contentType(mediaType).accept(acceptType).header("Authorization",authHeader).header(HttpHeaders.IF_MATCH, ifMatch).header(OSLCConstants.OSLC_CORE_VERSION,"2.0").header(HttpHeaders.IF_MATCH, ifMatch).put(artifact);
	}
	
	/**
	 * Create (POST) an artifact to a URL - usually an OSLC Creation Factory
	 * @param url
	 * @param artifact
	 * @param mediaType
	 * @param acceptType
	 * @return
	 * @throws URISyntaxException 
	 * @throws OAuthException 
	 * @throws IOException 
	 */
	public ClientResponse createResource(final String url, final Object artifact, String mediaType, String acceptType) throws IOException, OAuthException, URISyntaxException  {
		OAuthMessage message = getResourceInternal(url, HttpMethod.POST, false);
		
		String realm = "Jazz";
		// Change if a different name was detected
		if ( oauth_real_name != null ) {
			realm = oauth_real_name;
		}
		String authHeader = message.getAuthorizationHeader(realm);

		ClientConfig config = getClientConfig();

		javax.ws.rs.core.Application app = new javax.ws.rs.core.Application() {
		       public Set<Class<?>> getClasses() {
		           Set<Class<?>> classes = new HashSet<Class<?>>();
		           classes.addAll(JenaProvidersRegistry.getProviders());
		           
		           return classes;
		       }
		};
		config = config.applications(app);

		RestClient restClient = new RestClient(config);
		
		return restClient.resource(url).contentType(mediaType).accept(acceptType).header("Authorization",authHeader).header(OSLCConstants.OSLC_CORE_VERSION,"2.0").post(artifact);
		
		// return restClient.resource(url).accept(mediaType).header("Authorization",authHeader).header("OSLC-Core-Version", "2.0").get();
	}
	
	
	/**
	 * Performs necessary OAuth negotiation.
	 * 	- get request token
	 *  - throw redirect exception to authorization URL (and print message)
	 *  - exchange request token for access token
	 *  - access protected URL and return OAuthMessage  
	 * 
	 * @param url
	 * @param httpMethod
	 * @param restart
	 * @return
	 * @throws IOException
	 * @throws OAuthException
	 * @throws URISyntaxException
	 */
	private OAuthMessage getResourceInternal(String url, String httpMethod, boolean restart) throws IOException, OAuthException, URISyntaxException {

		OAuthClient client = new OAuthClient(new HttpClient4(this.getClientPool()));
		//No request token yet, get the request token and throw exception to redirect for authorization.
		if (accessor.requestToken == null) {
			client.getRequestToken(accessor);
			System.out.println("Enter this URL in a browser and run again: "+ accessor.consumer.serviceProvider.userAuthorizationURL +
					           "?oauth_token=" + accessor.requestToken);  // for command line use
			throw new OAuthRedirectException(accessor.consumer.serviceProvider.userAuthorizationURL, accessor);
		}
		
		//Exchange request token for access token. 
		if (accessor.accessToken == null) {
			try {
			   client.getAccessToken(accessor, OAuthMessage.POST, null);
			} catch (OAuthException e) {
				LOGGER.debug("OAuthException caught: " + e.getMessage());
				if (restart) 
				{
					LOGGER.error("Failed to get access key.");
					e.printStackTrace();
				} else {
					//restart the dance
					accessor.accessToken = null;
					accessor.requestToken = null;
					return getResourceInternal(url, httpMethod, true);
				}
			}
		}
		
	    OAuthMessage message = accessor.newRequestMessage(httpMethod, url, null);
		
		
		return message;
	}
	
	

	
}
