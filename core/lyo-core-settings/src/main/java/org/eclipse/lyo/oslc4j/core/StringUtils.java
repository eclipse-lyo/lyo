package org.eclipse.lyo.oslc4j.core;

import java.util.regex.Pattern;

public class StringUtils {
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
