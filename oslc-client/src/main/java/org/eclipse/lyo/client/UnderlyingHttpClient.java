package org.eclipse.lyo.client;

import javax.ws.rs.client.Client;
import org.apache.http.client.HttpClient;

/**
 * Returns the Apache HTTP client underlying the JAX-RS client.
 * @return the http client
 */
public interface UnderlyingHttpClient {
    public HttpClient get(Client client);
}
