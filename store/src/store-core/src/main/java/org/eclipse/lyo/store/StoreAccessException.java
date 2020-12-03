package org.eclipse.lyo.store;

/*-
 * #%L
 * Contributors:
 *     Andrew Berezovskyi - initial implementation
 * %%
 * Copyright (C) 2016 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

/**
 * StoreAccessException is an exception for read/write problems with the backing triplestore or
 * dataset.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.13
 */
public class StoreAccessException extends Exception {
    public StoreAccessException() {
        super();
    }

    public StoreAccessException(final String message) {
        super(message);
    }

    public StoreAccessException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public StoreAccessException(final Throwable cause) {
        super(cause);
    }

    protected StoreAccessException(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean stackTrace) {
        super(message, cause, enableSuppression, stackTrace);
    }
}
