/*
 * Copyright (c) 2019 KTH Royal Institute of Technology and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.trs.client.mqtt

import org.eclipse.lyo.trs.client.handlers.IProviderEventHandler
import org.eclipse.lyo.trs.client.handlers.IPushProviderHandler
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR
import org.slf4j.Logger
import org.slf4j.LoggerFactory


class MqttProviderHandler(private val eventHandler: IProviderEventHandler) : IPushProviderHandler {
    private val log: Logger = LoggerFactory.getLogger(MqttProviderHandler::class.java)
    override fun handlePush(eventMessage: ChangeEventMessageTR?) {
        eventHandler.handleChangeEvent(eventMessage);
    }

    override fun update() {
        log.debug("Skipping TRS update, this is a push-only handler")
    }
}
