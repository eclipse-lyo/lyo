package org.eclipse.lyo.oslc4j.trs.client.exceptions;

import org.apache.wink.client.ClientResponse;

/**
 * High-level exception for connection problems to the TRS Server endpoints.
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class TrsEndpointException extends Exception {

    private final ClientResponse response;

    public TrsEndpointException() {
        super();
        response = null;
    }

    public TrsEndpointException(final String message) {
        super(message);
        response = null;
    }

    public TrsEndpointException(final String message, final Throwable cause) {
        super(message, cause);
        response = null;
    }

    public TrsEndpointException(final Throwable cause) {
        super(cause);
        response = null;
    }

    public TrsEndpointException(final ClientResponse response) {
        super();
        this.response = response;
    }

    protected TrsEndpointException(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
        response = null;
    }

    public ClientResponse getResponse() {
        return response;
    }
}
