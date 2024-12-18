/*
 * Copyright (c) 2024 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.core.util;

import java.util.regex.Pattern;

/**
 * @since 7.0.0
 */
public class StringUtils {
    /**
     * Pattern to match control characters in the Unicode Cc category that are not CR, LF, or TAB
     */
    private static final Pattern CONTROL_CHAR_PATTERN = Pattern.compile("^\\p{Cc}&&[^\\r\\n\\t]+$");

    /**
     * Trim and strip control chars
     */
    public static String clean(String str) {
        if (str == null) return null;

        return CONTROL_CHAR_PATTERN.matcher(str).replaceAll("").trim();
    }

    /**
     * Trim and strip control chars; return an empty string if a null is encountered
     */
    public static String cleanNonNull(String str) {
        if (str == null) return "";

        return CONTROL_CHAR_PATTERN.matcher(str).replaceAll("").trim();
    }

    public static boolean isNullOrWhitespace(String str) {
        return str == null || str.isBlank();
    }

    public static boolean isNullOrEmpty(String str) {
        return str == null || str.isEmpty();
    }
}
