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

package org.eclipse.lyo.trs.client.handlers;

import org.eclipse.lyo.trs.client.model.BaseMember;
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR;

public class TestProviderHandler implements IProviderEventHandler {
    @Override
    public void finishCycle() {}

    @Override
    public void handleBaseMember(final BaseMember baseMember) {}

    @Override
    public void handleChangeEvent(final ChangeEventMessageTR eventMessageTR) {}

    @Override
    public void rebase() {}
}
