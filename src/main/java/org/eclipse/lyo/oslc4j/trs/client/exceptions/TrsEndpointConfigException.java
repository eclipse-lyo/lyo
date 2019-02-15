package org.eclipse.lyo.oslc4j.trs.client.exceptions;

import org.apache.wink.client.ClientResponse;

/**
 * TRS Client has a wrong configuration of a TRS Server endpoint.
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class TrsEndpointConfigException extends TrsEndpointException {
    public TrsEndpointConfigException() {
        super();
    }

    public TrsEndpointConfigException(final String message) {
        super(message);
    }

    public TrsEndpointConfigException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public TrsEndpointConfigException(final Throwable cause) {
        super(cause);
    }

    public TrsEndpointConfigException(final ClientResponse response) {
        super(response);
    }

    protected TrsEndpointConfigException(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
