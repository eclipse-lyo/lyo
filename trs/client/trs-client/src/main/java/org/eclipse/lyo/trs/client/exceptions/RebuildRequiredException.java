/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.trs.client.exceptions;

/**
 * Indicates that a provider's last processed change event is no longer available in the
 * change log, so the client's synchronization must be rebuilt from the server's base.
 * This can follow log truncation, a server rebase, or a server rollback.
 *
 * @since 2.3.0
 */
public class RebuildRequiredException extends RuntimeException {

    private static final long serialVersionUID = -5190311252768510792L;

    public RebuildRequiredException(String string) {
        super(string);
    }

}
