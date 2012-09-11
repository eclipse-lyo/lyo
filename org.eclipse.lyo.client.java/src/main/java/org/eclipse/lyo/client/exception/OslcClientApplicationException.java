/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.exception;

/**
 * Base class for all application exceptions.
 */
public abstract class OslcClientApplicationException extends Exception {
    private static final long serialVersionUID = -5933150329026674184L;

    public OslcClientApplicationException(final String messageKey, final Object[] args) {
        super(MessageExtractor.getMessage(messageKey, args));
    }

    public OslcClientApplicationException(final String messageKey, final Object[] args, final Throwable t) {
        super(MessageExtractor.getMessage(messageKey, args), t);
    }
}