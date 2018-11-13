/*******************************************************************************
 * Copyright (c) 2018 Andrew Berezovskyi.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *	   Andrew Berezovskyi - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena;

/**
 * An umbrella of exceptions that can be thrown by Jena or helper classes.
 * <p>
 * Examples include: DatatypeConfigurationException, IllegalAccessException,
 * IllegalArgumentException, InstantiationException, InvocationTargetException,
 * OslcCoreApplicationException, URISyntaxException, SecurityException, NoSuchMethodException
 *
 * @version $version-stub$
 * @since 2.4.0
 */
public class LyoJenaModelException extends Exception {
    public LyoJenaModelException() {
        super();
    }

    public LyoJenaModelException(final String message) {
        super(message);
    }

    public LyoJenaModelException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public LyoJenaModelException(final Throwable cause) {
        super(cause);
    }

    protected LyoJenaModelException(final String message, final Throwable cause,
            final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
