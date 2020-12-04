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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

package org.eclipse.lyo.trs.client.config

import java.net.URI

class TrsProviderConfiguration(val trsUri: URI, val basicAuthUsername: String?,
                               val basicAuthPassword: String?) {
    companion object {
        fun forHttp(trsEndpointUri: String): TrsProviderConfiguration {
            return TrsProviderConfiguration(URI.create(trsEndpointUri), null, null)
        }

        fun forHttpWithBasicAuth(trsEndpointUri: String, trsEndpointUsername: String,
                                 trsEndpointPassword: String): TrsProviderConfiguration {
            return TrsProviderConfiguration(URI.create(trsEndpointUri), trsEndpointUsername,
                    trsEndpointPassword)
        }
    }
}
