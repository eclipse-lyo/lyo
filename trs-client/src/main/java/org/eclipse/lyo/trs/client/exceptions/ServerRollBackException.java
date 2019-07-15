/*
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.trs.client.exceptions;

/**
 * Thrown normally by a TRS Provider when the last change event which was processed can not be found
 * in the list of change logs which was retrieved from the server meaning probably that the server
 * has been rolled back to a state previous to that change event
 *
 * @since 2.3.0
 */
public class ServerRollBackException extends RuntimeException {

    private static final long serialVersionUID = -5190311252768510792L;

    public ServerRollBackException(String string) {
        super(string);
    }

}
