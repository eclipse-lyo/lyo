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
package org.eclipse.lyo.oslc4j.core;

import java.util.HashMap;
import java.util.Map;

public interface OSLC4JConstants {

	String OSLC4J                            = "org.eclipse.lyo.oslc4j.";
	String OSLC4J_PUBLIC_URI                 = OSLC4J + "publicURI";
	String OSLC4J_DISABLE_HOST_RESOLUTION    = OSLC4J + "disableHostResolution";
	String OSLC4J_DISABLE_RELATIVE_URIS      = OSLC4J + "disableRelativeURIs";
	String OSLC4J_USE_BEAN_CLASS_FOR_PARSING = OSLC4J + "useBeanClassForParsing";
	String OSLC4J_INFER_TYPE_FROM_SHAPE      = OSLC4J + "inferTypeFromResourceShape";

	Map<String, Object> OSL4J_PROPERTY_SINGLETON = new HashMap<>(0);

	String OSLC4J_SELECTED_PROPERTIES = OSLC4J + "selected.properties";
	String OSLC4J_NEXT_PAGE           = OSLC4J + "next.page";
	String OSLC4J_TOTAL_COUNT         = OSLC4J + "total.count";

	/**
	 * System property {@value} : When "true", the query result list type will be
	 * http://www.w3.org/2000/01/rdf-schema#Container, otherwise it will have no type. No type is
	 * the default.
	 */
	String OSLC4J_QUERY_RESULT_LIST_AS_CONTAINER = OSLC4J + "queryResultListAsContainer";

	/**
	 * System property {@value} : When "true", always abbreviate RDF/XML, even when asked for
	 * application/rdf+xml. Otherwise, abbreviated RDF/XML is only returned when application/xml is
	 * requested. Does not affect text/turtle responses.
	 */
	String OSLC4J_ALWAYS_XML_ABBREV = OSLC4J + "alwaysXMLAbbrev";

	/**
	 * System property {@value} : When "true" (default), fail on when reading a property value that
	 * is not a legal instance of a datatype. When "false", skip over invalid values in extended
	 * properties.
	 */
	String OSLC4J_STRICT_DATATYPES = OSLC4J + "strictDatatypes";

    /**
     * System property {@value} : When "false" (default), add an OrderBy clause to the queries in LyoStore that involve paging.
     * When "true", do not add such an OrderBy clause, in the hope that the triplestore sorting algorithm is stable-
     * properties.
     */
    String LYO_STORE_PAGING_UNSAFE = OSLC4J + "storePagingUnsafe";

    /**
     * System property {@value} : When "true" (default), expect the store to return exactly the number of entries in the request when dealing with paging.
     * When "false", expect the store to return an additional element to indicate that additional pages exist.
     */
    String LYO_STORE_PAGING_PRECISE_LIMIT = OSLC4J + "storePagingPreciseLimit";

}
