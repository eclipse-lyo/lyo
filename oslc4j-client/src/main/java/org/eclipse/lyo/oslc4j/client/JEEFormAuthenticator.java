package org.eclipse.lyo.oslc4j.client;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.client.ClientResponseContext;
import javax.ws.rs.client.ClientResponseFilter;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;

import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;

/**
 * A filter that can be registered in order to non-preemptively handle JEE Form
 * based authentication challenges.
 * 
 * @author jamsden
 *
 */
public class JEEFormAuthenticator implements ClientRequestFilter, ClientResponseFilter {
    private static final String COOKIE = "Cookie";
 
    // security params
    private static final String J_SECURITY_CHECK = "j_security_check";
    private static final String J_USERNAME = "j_username";
    private static final String J_PASSWORD = "j_password";
    private static final String FORM_AUTHENTICATOR_REUSED = "org.eclipse.lyo.client.oslc.JEEFormAuthenticator.reused";
	private static final String JAZZ_AUTH_MESSAGE_HEADER = "X-com-ibm-team-repository-web-auth-msg";
	private static final String JAZZ_AUTH_REQUIRED = "authrequired";
	private static final String JAZZ_AUTH_FAILED = "authfailed";
 
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
 
    /**
     * @param baseUri base URI for the server, e.g., https://host:9443/ccm
     * @param username user's credentials
     * @param password
     */
    public JEEFormAuthenticator(final String baseUri, final String username, final String password) {
    	this.userId = username;
        this.password = password;
        this.baseUri = baseUri;
    }

	/* (non-Javadoc)
	 * Checks to see if the response is a 401 UNAUTHORIZED. If so, it attempts to 
	 * authenticate the user, and then retries the request with the updated 
	 * session information.
	 * 
	 * @see javax.ws.rs.client.ClientResponseFilter#filter(javax.ws.rs.client.ClientRequestContext, javax.ws.rs.client.ClientResponseContext)
	 */
	@Override
    public void filter(ClientRequestContext request, ClientResponseContext response) throws IOException {
        final List<Object> cookies = new ArrayList<>();
        
        boolean authRequired = JAZZ_AUTH_REQUIRED.equals(response.getHeaderString(JAZZ_AUTH_MESSAGE_HEADER));
        boolean authAlreadyAttempted = "true".equals(request.getProperty(FORM_AUTHENTICATOR_REUSED));

        if (authRequired && authAlreadyAttempted) {
        	response.setStatus(Response.Status.UNAUTHORIZED.getStatusCode());
        	return;
        } else if (!authRequired) {
        	return;
        }
        // We got an Authentication challenge, attempt to authenticate the user
        authClient = request.getClient();

        final Form form = new Form();
        form.param(J_USERNAME, this.userId);
        form.param(J_PASSWORD, this.password);
        Response authResponse = authClient
        	.target(this.baseUri)
        	.path(J_SECURITY_CHECK)
            .request(MediaType.APPLICATION_FORM_URLENCODED)
            .header("Accept", "*/*")
            .header("X-Requested-With", "XMLHttpRequest")
    		.header("OSLC-Core-Version", "2.0")
            .post(Entity.form(form));
        authResponse.getCookies().values().stream().forEach((cookie) -> {
            cookies.add(cookie.toCookie());
        });
		int statusCode = authResponse.getStatus();
		// Check the result
		String jazzAuthMessage = authResponse.getHeaderString(JAZZ_AUTH_MESSAGE_HEADER);
		
		if (jazzAuthMessage != null && jazzAuthMessage.equalsIgnoreCase(JAZZ_AUTH_FAILED)) {
			authResponse.close();
        	response.setStatus(Response.Status.UNAUTHORIZED.getStatusCode());
        	return;
		}
		
		String location = authResponse.getHeaderString("Location");
		authResponse.close();
		statusCode = followRedirects(statusCode, location);
 
		// retry the request with the updated cookies
		Client requestClient = request.getClient();
		Invocation.Builder retryBuilder = requestClient
			.target(request.getUri())
			.request(request.getMediaType());
        retryBuilder.property(FORM_AUTHENTICATOR_REUSED, "true"); // prevent infinite loops
        MultivaluedMap<String, Object> newHeaders = new MultivaluedHashMap<String, Object>();
        newHeaders.putAll(request.getHeaders());
        newHeaders.add(COOKIE, cookies);        
		retryBuilder.headers(newHeaders);
        Invocation invocation = null;
        String requestMethod = request.getMethod();
        if (request.getEntity() == null) {
            invocation = retryBuilder.build(requestMethod);
        } else {
            invocation = retryBuilder.build(requestMethod,
                    Entity.entity(request.getEntity(), request.getMediaType()));
        }
        Response retryResponse = invocation.invoke();

        if (retryResponse.hasEntity()) {
            response.setEntityStream(retryResponse.readEntity(InputStream.class));
        }
        MultivaluedMap<String, String> headers = response.getHeaders();
        headers.clear();
        headers.putAll(retryResponse.getStringHeaders());
        response.setStatus(retryResponse.getStatus());			
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

	@Override
	public void filter(ClientRequestContext requestContext) throws IOException {
		// do nothing, JEE Form is always preemptive
	}
}
