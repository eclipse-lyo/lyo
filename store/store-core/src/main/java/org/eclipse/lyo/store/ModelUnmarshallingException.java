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
 * ModelUnmarshallingException is an exception that occurs during
 * {@link org.apache.jena.rdf.model.Model}
 * transformation.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public class ModelUnmarshallingException extends Exception {
    public ModelUnmarshallingException() {
        super();
    }

    public ModelUnmarshallingException(final String message) {
        super(message);
    }

    public ModelUnmarshallingException(final String message, final Throwable cause) {
        super(message, cause);
    }

    public ModelUnmarshallingException(final Throwable cause) {
        super(cause);
    }

    protected ModelUnmarshallingException(final String message, final Throwable cause,
            final boolean enableSuppression, final boolean stackTrace) {
        super(message, cause, enableSuppression, stackTrace);
    }
}
