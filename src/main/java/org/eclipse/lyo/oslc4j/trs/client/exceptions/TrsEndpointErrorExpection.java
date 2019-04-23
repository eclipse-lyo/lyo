package org.eclipse.lyo.oslc4j.trs.client.exceptions;


/**
 * TRS Server endpoint returns a server error.
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class TrsEndpointErrorExpection extends TrsEndpointException {
    public TrsEndpointErrorExpection() {
        super();
    }

    public TrsEndpointErrorExpection(final String message) {
        super(message);
    }

    public TrsEndpointErrorExpection(final String message, final Throwable cause) {
        super(message, cause);
    }

    public TrsEndpointErrorExpection(final Throwable cause) {
        super(cause);
    }

    protected TrsEndpointErrorExpection(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
