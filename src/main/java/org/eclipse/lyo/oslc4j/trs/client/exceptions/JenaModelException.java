package org.eclipse.lyo.oslc4j.trs.client.exceptions;

/**
 * Created on 2018-02-27
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public class JenaModelException extends Exception {

    public JenaModelException() {
    }

    public JenaModelException(final String message) {
        super(message);
    }

    public JenaModelException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public JenaModelException(final Throwable cause) {
        super(cause);
    }

    public JenaModelException(final String message, final Throwable cause, final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
