package org.eclipse.lyo.trs.client.exceptions;


/**
 * TRS Server endpoint returns a server error.
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class TrsEndpointErrorException extends TrsEndpointException {
    public TrsEndpointErrorException() {
        super();
    }

    public TrsEndpointErrorException(final String message) {
        super(message);
    }

    public TrsEndpointErrorException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public TrsEndpointErrorException(final Throwable cause) {
        super(cause);
    }

    protected TrsEndpointErrorException(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
