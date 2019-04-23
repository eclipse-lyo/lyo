package org.eclipse.lyo.oslc4j.trs.client.exceptions;

import javax.xml.ws.Response;

/**
 * High-level exception for connection problems to the TRS Server endpoints.
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class TrsEndpointException extends Exception {

    public TrsEndpointException() {
        super();
    }

    public TrsEndpointException(final String message) {
        super(message);
    }

    public TrsEndpointException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public TrsEndpointException(final Throwable cause) {
        super(cause);
    }

    protected TrsEndpointException(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
