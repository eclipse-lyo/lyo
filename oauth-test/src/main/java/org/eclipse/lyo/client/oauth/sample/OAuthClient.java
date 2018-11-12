/*******************************************************************************
 * Copyright (c) 2014 IBM Corporation.
 *
 *	All rights reserved. This program and the accompanying materials
 *	are made available under the terms of the Eclipse Public License v1.0
 *	and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 *	The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *	and the Eclipse Distribution License is available at
 *	http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *	Contributors:
 *
 *	   Samuel Padgett	   - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oauth.sample;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import net.oauth.OAuthAccessor;
import net.oauth.OAuthConsumer;
import net.oauth.OAuthException;
import net.oauth.OAuthMessage;
import net.oauth.OAuthServiceProvider;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.conn.ssl.X509HostnameVerifier;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.EntityUtils;

/**
 * A simple client example for two-legged OAuth authentication. It signs
 * requests using the consumer key and secret in
 * {@code src/main/resources/oauth.properties} and dumps the response to the
 * console.
 *
 * <p>Usage: {@code java org.eclipse.lyo.client.oauth.sample.OAuthClient <uri>}
 */
public class OAuthClient
{
	private static Properties oAuthProperties = new Properties();
	static {
		try {
			oAuthProperties.load(OAuthClient.class.getResourceAsStream("/oauth.properties"));
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	public static void main(final String[] args) {
		if (args.length == 0) {
			System.err.println("Usage: java org.eclipse.lyo.client.oauth.sample.OAuthClient <uri>");
			System.exit(1);
		}

		final String uri = args[0];
		try {
			get(uri);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
	}

	public static void get(final String uri) throws OAuthException, IOException, URISyntaxException {
		final HttpClient client = getClient();
		final OAuthAccessor accessor = getOAuthAccessor();
		final OAuthMessage message = accessor.newRequestMessage("GET", uri, null);
		final String authHeader = message.getAuthorizationHeader(null);

		System.out.println("GET " + uri);
		System.out.println("Authorization: " + authHeader);
		System.out.println();

		final HttpGet get = new HttpGet(uri);
		get.setHeader("Accept", "application/rdf+xml");
		get.setHeader("Authorization", authHeader);
		get.setHeader("OSLC-Core-Version", "2.0");

		// execute the HTTP request and dump the response to the console
		HttpResponse response = client.execute(get);
		System.out.println(response.getStatusLine());
		for (Header h : response.getAllHeaders()) {
			System.out.println(h.getName() + ": " + h.getValue());
		}
		System.out.println();
		System.out.println(EntityUtils.toString(response.getEntity()));
	}

	private static OAuthAccessor getOAuthAccessor() {
		final String key = oAuthProperties.getProperty("consumer_key");
		// treat the secret like a password!
		final String secret = oAuthProperties.getProperty("consumer_secret");

		// the constructor properties for OAuthServiceProvider are not required for two-legged OAuth
		final OAuthServiceProvider provider = new OAuthServiceProvider(null, null, null);
		final OAuthConsumer consumer = new OAuthConsumer("", key, secret, provider);
		final OAuthAccessor accessor = new OAuthAccessor(consumer);
		accessor.accessToken = "";

		return accessor;
	}

	private static HttpClient getClient() {
		final HttpClient client = new DefaultHttpClient();
		disableCertificateValidatation(client);

		return client;
	}

	// not recommended for production environments!
	// needed if the server uses self-signed certificates, however
	private static void disableCertificateValidatation(HttpClient client) {
		try {
			final SSLContext sc = SSLContext.getInstance("SSL");
			sc.init(null, new TrustManager[] {
				new X509TrustManager() {
					public java.security.cert.X509Certificate[] getAcceptedIssuers() { return new X509Certificate[0]; }
					public void checkClientTrusted( java.security.cert.X509Certificate[] certs, String authType) { }
					public void checkServerTrusted( java.security.cert.X509Certificate[] certs, String authType) { }
				}
			}, new java.security.SecureRandom());
			final SSLSocketFactory socketFactory = new SSLSocketFactory(sc, new X509HostnameVerifier() {
				public void verify(String string, SSLSocket ssls) throws IOException {}
				public void verify(String string, X509Certificate xc) throws SSLException {}
				public void verify(String string, String[] strings, String[] strings1) throws SSLException {}
				public boolean verify(String string, SSLSession ssls) {
					return true;
				}
			});
			final Scheme https = new Scheme("https", 443, socketFactory);
			client.getConnectionManager().getSchemeRegistry().register(https);
		} catch (GeneralSecurityException e) {}
	}
}
