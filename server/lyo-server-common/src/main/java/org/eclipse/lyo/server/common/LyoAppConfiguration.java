/*
 * Copyright (c) 2025 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.server.common;

import java.util.Set;

/**
 * Configuration record for Eclipse Lyo server apps
 *
 * @param baseUrl Base URI of the application.
 * @param servletPath Further base prefix for the servlet (if any)
 * @param corsFriends A set of "friendly" endpoints (origins) permitted to make
 *              CORS requests. <code>*</code> wildcard permits any origin to
 *              make CORS requests.
 */
public record LyoAppConfiguration(String baseUrl, String servletPath, Set<String> corsFriends) {}
