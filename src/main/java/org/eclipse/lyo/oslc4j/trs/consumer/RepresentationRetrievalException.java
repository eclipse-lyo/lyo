/*
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 *
 * Contributors:
 *
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.consumer;

/**
 * Thrown when the required representation can not be retrieved from the server. Normally thrown by
 * a Base member handler or a change event handler
 *
 * @since 2.3.0
 */
public class RepresentationRetrievalException extends Exception {
    private static final long serialVersionUID = -5190311252768510792L;
    private static String message = "The representation of one of the resources could not be " +
            "retireved while procesing the tracked resource set !";

    public RepresentationRetrievalException() {
        super(message);
    }

    public RepresentationRetrievalException(String string) {
        super(message + "\n" + string);
    }

}
