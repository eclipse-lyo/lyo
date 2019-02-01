package org.eclipse.lyo.client.oslc;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.ssl.SSLContextBuilder;

public class JEEFormAuthenticator  implements ClientRequestFilter {
    private static final String COOKIE = "Cookie";
 
    // security params
    private static final String J_SECURITY_CHECK = "j_security_check";
    private static final String J_USERNAME = "j_username";
    private static final String J_PASSWORD = "j_password";
 
    private final String username;
    private final String password;
    private final String baseUri;
     
    // requires by @Provider
    public JEEFormAuthenticator() {
        this.username = null;
        this.password = null;
        this.baseUri = null;
    }
 
    public JEEFormAuthenticator(final String baseUri, final String username, final String password) {
    	this.username = username;
        this.password = password;
        this.baseUri = baseUri;
    }
 
    @Override
    public void filter(final ClientRequestContext requestContext) throws IOException {
        final List<Object> cookies = new ArrayList<>();
        
		// Setup SSL support to ignore self-assigned SSL certificates - for testing only!!
		ClientBuilder clientBuilder = ClientBuilder.newBuilder();		
	    SSLContextBuilder sslContextBuilder = null;
	    try {
		    sslContextBuilder = new SSLContextBuilder();
		    sslContextBuilder.loadTrustMaterial(TrustSelfSignedStrategy.INSTANCE);
		    clientBuilder.sslContext(sslContextBuilder.build());
	    } catch (Exception e) {
	    }
	    clientBuilder.hostnameVerifier(NoopHostnameVerifier.INSTANCE);

 
        /*
         * This is hitting the URL as requested by the ClientBuilder.
         * (Refer to the line: "1. Send GET request to the needed private resource,
         * in response you get a cookie (Header “Set cookie”)"
         */
        final Client initialClient =  clientBuilder.build();
        final Response responseInitial = initialClient
                .target(requestContext.getUri())
                .request(requestContext.getAcceptableMediaTypes().get(0))
                .method(requestContext.getMethod());
        initialClient.close();
 
        /*
         * This section is getting the cookie above and use it to make the call against jsecuritycheck
         * (Refer to the line: 2. Send request with cookie (from step 1) to the j_security_check.
         * On response you should get code 302 – “Moved Temporarily”)
         */
        responseInitial.getCookies().values().stream().forEach((cookie) -> {
            cookies.add(cookie.toCookie());
        });
        final Client loginClient = clientBuilder.build();
        final Form form = new Form();
        form.param(J_USERNAME, this.username);
        form.param(J_PASSWORD, this.password);
        final Response loginResponse = loginClient.target(this.baseUri).path(J_SECURITY_CHECK)
                .request(MediaType.APPLICATION_FORM_URLENCODED)
                .header(COOKIE, cookies)
                .post(Entity.form(form));
        loginClient.close();
        loginResponse.getCookies().values().stream().forEach((cookie) -> {
            cookies.add(cookie.toCookie());
        });
 
        /*
         * This is right before making the actual call. So we add the cookie (which the server will be
         * able to tell that this call has been authenticated).
         * (Refer to the line: 3. Now you can repeat request to the private resource with same cookie,
         * on response you get needed resource data.)
         */
        requestContext.getHeaders().put(COOKIE, cookies);
    }
}
