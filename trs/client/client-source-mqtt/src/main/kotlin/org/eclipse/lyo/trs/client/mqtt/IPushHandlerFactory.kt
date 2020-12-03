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

import org.eclipse.lyo.core.trs.TrackedResourceSet
import org.eclipse.lyo.trs.client.handlers.IPushProviderHandler

// FIXME Andrew@2019-07-21: replace all the topic parameters with a factory
interface IPushHandlerFactory {
    fun handlerFor(topic: String): IPushProviderHandler
    fun handlerFor(provider: TrackedResourceSet): IPushProviderHandler
}
