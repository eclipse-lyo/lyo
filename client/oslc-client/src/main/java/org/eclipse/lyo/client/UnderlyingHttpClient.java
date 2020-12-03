/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-1.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.client;

import javax.ws.rs.client.Client;
import org.apache.http.client.HttpClient;


public interface UnderlyingHttpClient {
    /**
     * Returns the Apache HTTP client underlying the JAX-RS client.
     * @return the http client
     */
    public HttpClient get(Client client);
}
