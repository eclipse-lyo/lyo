/*
 * Copyright (c) 2023 Contributors to the Eclipse Foundation
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

package org.eclipse.lyo.trs.client.config;

import com.google.common.base.Strings;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Properties;

/**
 * Loads TRS Provider configuration from a .properties file.
 * <p>
 * Supported properties:
 * <ul>
 *     <li><code>trs_uri</code> - a TRS Provider endpoint</li>
 *     <li><code>baseAuth_user</code> - HTTP Basic auth user (optional)</li>
 *     <li><code>baseAuth_pwd</code> - HTTP Basic auth password (optional)</li>
 * </ul>
 *
 * @since 4.0.0
 */
public class TrsConfigurationLoader {
    public static TrsProviderConfiguration from(File f) throws IOException {
        if (f == null) {
            throw new IllegalArgumentException("File is null");
        }

        try (InputStream input = new BufferedInputStream(new FileInputStream(f))) {
            Properties p = new Properties();
            p.load(input);

            String trsUriParam = p.getProperty("trs_uri");
            if (Strings.isNullOrEmpty(trsUriParam)) {
                throw new IllegalStateException(
                        "The 'trs_uri' field is missing in file " + f.getName());
            }

            String user = p.getProperty("baseAuth_user");
            String pass = p.getProperty("baseAuth_pwd");

            return new TrsProviderConfiguration(URI.create(trsUriParam), user, pass);
        }
    }
}
