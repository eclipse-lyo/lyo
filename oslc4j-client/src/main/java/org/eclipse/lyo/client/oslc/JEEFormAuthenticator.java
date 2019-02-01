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

import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.ssl.SSLContextBuilder;

public class JEEFormAuthenticator  implements ClientRequestFilter {
    private static final String COOKIE = "Cookie";
 
    // security params
    private static final String J_SECURITY_CHECK = "j_security_check";
    private static final String J_USERNAME = "j_username";
    private static final String J_PASSWORD = "j_password";
 
    private final String userId;
    private final String password;
    private final String baseUri;
	private Response lastRedirectResponse = null;
    Client authClient = null;
    
     
    // requires by @Provider
    public JEEFormAuthenticator() {
        this.userId = null;
        this.password = null;
        this.baseUri = null;
    }
 
    public JEEFormAuthenticator(final String baseUri, final String username, final String password) {
    	this.userId = username;
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
        authClient =  clientBuilder.build();
        Response response = authClient
                .target(this.baseUri + "/authenticated/identity")
                .request()
                .get();
		int statusCode = response.getStatus();
		String location = response.getHeaderString("Location");
		response.close();
		statusCode = followRedirects(statusCode, location);

        /*
         * This section is getting the cookie above and use it to make the call against jsecuritycheck
         * (Refer to the line: 2. Send request with cookie (from step 1) to the j_security_check.
         * On response you should get code 302 – “Moved Temporarily”)
         */
        response.getCookies().values().stream().forEach((cookie) -> {
            cookies.add(cookie.toCookie());
        });
        
        final Form form = new Form();
        form.param(J_USERNAME, this.userId);
        form.param(J_PASSWORD, this.password);
        response = authClient
        	.target(this.baseUri).path(J_SECURITY_CHECK)
            .request(MediaType.APPLICATION_FORM_URLENCODED)
            .header("Accept", "*/*")
            .header("X-Requested-With", "XMLHttpRequest")
    		.header("Content-Type", "application/x-www-form-urlencoded; charset=utf-8")
    		.header("OSLC-Core-Version", "2.0")
            .post(Entity.form(form));
        response.getCookies().values().stream().forEach((cookie) -> {
            cookies.add(cookie.toCookie());
        });
		statusCode = response.getStatus();		
		location = response.getHeaderString("Location");
		response.close();
		statusCode = followRedirects(statusCode,location);
 
        /*
         * This is right before making the actual call. So we add the cookie (which the server will be
         * able to tell that this call has been authenticated).
         * (Refer to the line: 3. Now you can repeat request to the private resource with same cookie,
         * on response you get needed resource data.)
         */
        requestContext.getHeaders().put(COOKIE, cookies);
    }

	private int followRedirects(int statusCode, String location) throws ClientProtocolException, IOException
	{
		while ( ((statusCode == HttpStatus.SC_MOVED_TEMPORARILY) || (HttpStatus.SC_SEE_OTHER == statusCode)) && (location != null))
		{
			lastRedirectResponse = authClient.target(location).request().get();
			statusCode = lastRedirectResponse.getStatus();
			location = lastRedirectResponse.getHeaderString("Location");
			lastRedirectResponse.close();
		}
	
		return statusCode;
	}
}
