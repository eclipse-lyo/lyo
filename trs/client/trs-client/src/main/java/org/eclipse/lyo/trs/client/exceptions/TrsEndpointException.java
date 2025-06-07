package org.eclipse.lyo.trs.client.exceptions;

/**
 * High-level exception for connection problems to the TRS Server endpoints.
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class TrsEndpointException extends RuntimeException {

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

    protected TrsEndpointException(
            final String message,
            final Throwable cause,
            final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
