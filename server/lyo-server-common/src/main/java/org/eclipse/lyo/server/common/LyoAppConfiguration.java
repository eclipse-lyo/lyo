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

import java.util.Optional;
import java.util.Set;

import jakarta.servlet.ServletContext;
import org.eclipse.microprofile.config.Config;
import org.eclipse.microprofile.config.ConfigProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Configuration record for Eclipse Lyo server apps.
 *
 * @param baseUrl Base URI of the application.
 * @param servletPath Further base prefix for the servlet (if any)
 * @param corsFriends A set of "friendly" endpoints (origins) permitted to make
 *              CORS requests. <code>*</code> wildcard permits any origin to
 *              make CORS requests.
 */
public record LyoAppConfiguration(String baseUrl, String servletPath, Set<String> corsFriends) {

    private static final Logger logger = LoggerFactory.getLogger(LyoAppConfiguration.class);

    /**
     * Retrieves a configuration property using MicroProfile Config, falling back to
     * ServletContext init parameters if the property is not present.
     *
     * @param context the ServletContext to check for fallback (can be null)
     * @param key the configuration key
     * @return the property value, or null if not found
     */
    public static String getOslcConfigProperty(ServletContext context, String key) {
        try {
            Config config = ConfigProvider.getConfig();
            Optional<String> value = config.getOptionalValue(key, String.class);
            if (value.isPresent()) {
                logger.debug("Found property '{}' in MicroProfile Config", key);
                return value.get();
            }
        } catch (Exception e) {
            // ConfigProvider might fail if no implementation is available.
            logger.debug("MicroProfile Config not available or failed for key '{}': {}", key, e.getMessage());
        }

        if (context != null) {
            String initParam = context.getInitParameter(key);
            if (initParam != null) {
                logger.debug("Found property '{}' in ServletContext", key);
                return initParam;
            }
        }

        logger.debug("Property '{}' not found in configuration or ServletContext", key);
        return null;
    }
}
