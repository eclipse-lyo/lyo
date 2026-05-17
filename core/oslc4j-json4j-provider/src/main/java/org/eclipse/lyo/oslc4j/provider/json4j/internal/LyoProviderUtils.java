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
package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.core.UriBuilder;

import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;

/*
 * Extracted from org.eclipse.lyo.oslc4j.core.OSLC4JUtils,
 * to remove dependency on org.apache.jena.ext.com.google.common.base.Strings,
 * which is no longer provided by Jena 4.9
 */
public class LyoProviderUtils {

    /**
     * Returns the boolean value of the system property {@code org.eclipse.lyo.oslc4j.disableRelativeURIs}.
     * Default is {@code true} if not set (relative URIs will not be allowed)
     *
     * @return true if relative URIs are disabled, or if the property is not set.
     */
    public static boolean relativeURIsAreDisabled() {
        return Boolean.parseBoolean(System.getProperty(OSLC4JConstants.OSLC4J_DISABLE_RELATIVE_URIS, Boolean.TRUE.toString()));
    }

    /**
     * Resolve a URI, usually a resource subject or info URI.
     * Query parameters from the request are not copied to the resolved URI.
     *
     * @param request
     *            request to base resolved URI on
     * @return String containing the resolved URI
     */
    public static String resolveURI(HttpServletRequest request) {
        return UriBuilder.fromPath(request.getContextPath() + request.getServletPath() + request.getPathInfo()) //
                .scheme(request.getScheme()) //
                .host(request.getServerName()) //
                .port(request.getServerPort()) //
                .build().normalize().toString();
    }

    /**
     * Returns the boolean value of the system property {@code org.eclipse.lyo.oslc4j.useBeanClassForParsing}.
     * Default is {@code false} if not set.
     * Setting to {@code true} should be used for matching the resource {@code rdf:type} to
     * the {@code describes} parameter of the {@code OslcResourceShape} annotation.
     *
     * @return true if Java bean class must be used as is for binding, false otherwise.
     */
    /*
     * See https://bugs.eclipse.org/bugs/show_bug.cgi?id=412755
     */
    public static boolean useBeanClassForParsing() {
        return Boolean.getBoolean(OSLC4JConstants.OSLC4J_USE_BEAN_CLASS_FOR_PARSING);
    }

    /**
     * Return if the query result list type will be http://www.w3.org/2000/01/rdf-schema#Container
     * or there will be no type. Default is no type.
     */
    public static boolean isQueryResultListAsContainer() {
        return Boolean.getBoolean(OSLC4JConstants.OSLC4J_QUERY_RESULT_LIST_AS_CONTAINER);
    }

    private LyoProviderUtils() {
    }
}
