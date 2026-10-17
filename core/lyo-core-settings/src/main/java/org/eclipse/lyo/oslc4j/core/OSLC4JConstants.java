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

  String LYO_PROPERTY_PREFIX = "org.eclipse.lyo.oslc4j.";

  String LYO_PUBLIC_URI = LYO_PROPERTY_PREFIX + "publicURI";

  String LYO_USE_BEAN_CLASS_FOR_PARSING = LYO_PROPERTY_PREFIX + "useBeanClassForParsing";

  String LYO_INFER_TYPE_FROM_SHAPE = LYO_PROPERTY_PREFIX + "inferTypeFromResourceShape";

  /**
   * System property {@value} : When "true" (default), fail on when reading a property value that
   * is not a legal instance of a datatype. When "false", skip over invalid values in extended
   * properties.
   */
  String LYO_STRICT_DATATYPES = LYO_PROPERTY_PREFIX + "strictDatatypes";

  String LYO_DISABLE_RELATIVE_URIS = LYO_PROPERTY_PREFIX + "disableRelativeURIs";

  String LYO_DISABLE_HOST_RESOLUTION = LYO_PROPERTY_PREFIX + "disableHostResolution";

  /**
   * System property {@value} : When "true", the query result list type will be
   * http://www.w3.org/2000/01/rdf-schema#Container, otherwise it will have no type. No type is
   * the default.
   */
  String LYO_QUERY_RESULT_LIST_AS_CONTAINER = LYO_PROPERTY_PREFIX + "queryResultListAsContainer";

  /**
   * System property {@value} : When "false" (default), add an OrderBy clause to the queries in LyoStore that involve paging.
   * When "true", do not add such an OrderBy clause, in the hope that the triplestore sorting algorithm is stable-
   * properties.
   */
  String LYO_STORE_PAGING_UNSAFE = LYO_PROPERTY_PREFIX + "storePagingUnsafe";

  /**
   * System property {@value} : When "true" (default), expect the store to return exactly the number of entries in the request when dealing with paging.
   * When "false", expect the store to return an additional element to indicate that additional pages exist.
   */
  String LYO_STORE_PAGING_PRECISE_LIMIT = LYO_PROPERTY_PREFIX + "storePagingPreciseLimit";

  /*Properties below not used in OSLC4JUtils*/

  Map<String, Object> LYO_PROPERTY_SINGLETON = new HashMap<>(0);

  String LYO_SELECTED_PROPERTIES = LYO_PROPERTY_PREFIX + "selected.properties";
  String LYO_NEXT_PAGE = LYO_PROPERTY_PREFIX + "next.page";
  String LYO_TOTAL_COUNT = LYO_PROPERTY_PREFIX + "total.count";

  /**
   * System property {@value} : When "true", always abbreviate RDF/XML, even when asked for
   * application/rdf+xml. Otherwise, abbreviated RDF/XML is only returned when application/xml is
   * requested. Does not affect text/turtle responses.
   */
  String LYO_ALWAYS_XML_ABBREV = LYO_PROPERTY_PREFIX + "alwaysXMLAbbrev";

  /**
   * System property {@value}: When "true" (default is "false"), use abbreviated RDF/XML
   * for ServiceProvider and ServiceProviderCatalog resources requested as application/rdf+xml.
   */
  String LYO_ALWAYS_XML_ABBREV_ONLY_PROVIDERS =
      LYO_PROPERTY_PREFIX + "alwaysXMLAbbrevOnlyProviders";

  /**
   * System property {@value}: When "true" (default: {@value #LYO_PRETTY_PRINT_DEFAULT}),
   * pretty-print RIOT-backed RDF responses.
   * Set to "false" in production to use streaming writers for Turtle and flat JSON-LD.
   * RDF/XML uses the plain writer when disabled. The custom abbreviated XML writer is unaffected.
   * Jazz compatibility takes precedence over pretty printing for RDF/XML.
   */
  String LYO_PRETTY_PRINT = LYO_PROPERTY_PREFIX + "prettyPrint";

  /** Default value for the {@link #LYO_PRETTY_PRINT} system property: {@value}. */
  String LYO_PRETTY_PRINT_DEFAULT = "true";

  /**
   * System property {@value}: When "true" (default: {@value #LYO_JAZZ_COMPATIBILITY_DEFAULT}),
   * use Jena's RDFXML_PLAIN writer for
   * application/rdf+xml and text/xml, regardless of the pretty-print setting, because Jazz
   * sometimes cannot consume Jena's pretty RDF/XML. Set to "false" to allow Jena's pretty writer.
   * Explicit abbreviated XML settings and application/xml continue to use the custom writer.
   */
  String LYO_JAZZ_COMPATIBILITY = LYO_PROPERTY_PREFIX + "jazzCompatibility";

  /** Default value for the {@link #LYO_JAZZ_COMPATIBILITY} system property: {@value}. */
  String LYO_JAZZ_COMPATIBILITY_DEFAULT = "true";

  /** @deprecated Use {@link #LYO_PROPERTY_PREFIX} instead. */
  @Deprecated String OSLC4J = LYO_PROPERTY_PREFIX;

  /** @deprecated Use {@link #LYO_PUBLIC_URI} instead. */
  @Deprecated String OSLC4J_PUBLIC_URI = LYO_PUBLIC_URI;

  /** @deprecated Use {@link #LYO_USE_BEAN_CLASS_FOR_PARSING} instead. */
  @Deprecated String OSLC4J_USE_BEAN_CLASS_FOR_PARSING = LYO_USE_BEAN_CLASS_FOR_PARSING;

  /** @deprecated Use {@link #LYO_INFER_TYPE_FROM_SHAPE} instead. */
  @Deprecated String OSLC4J_INFER_TYPE_FROM_SHAPE = LYO_INFER_TYPE_FROM_SHAPE;

  /** @deprecated Use {@link #LYO_STRICT_DATATYPES} instead. */
  @Deprecated String OSLC4J_STRICT_DATATYPES = LYO_STRICT_DATATYPES;

  /** @deprecated Use {@link #LYO_DISABLE_RELATIVE_URIS} instead. */
  @Deprecated String OSLC4J_DISABLE_RELATIVE_URIS = LYO_DISABLE_RELATIVE_URIS;

  /** @deprecated Use {@link #LYO_DISABLE_HOST_RESOLUTION} instead. */
  @Deprecated String OSLC4J_DISABLE_HOST_RESOLUTION = LYO_DISABLE_HOST_RESOLUTION;

  /** @deprecated Use {@link #LYO_QUERY_RESULT_LIST_AS_CONTAINER} instead. */
  @Deprecated String OSLC4J_QUERY_RESULT_LIST_AS_CONTAINER = LYO_QUERY_RESULT_LIST_AS_CONTAINER;

  /** @deprecated Use {@link #LYO_PROPERTY_SINGLETON} instead. */
  @Deprecated Map<String, Object> OSL4J_PROPERTY_SINGLETON = LYO_PROPERTY_SINGLETON;

  /** @deprecated Use {@link #LYO_SELECTED_PROPERTIES} instead. */
  @Deprecated String OSLC4J_SELECTED_PROPERTIES = LYO_SELECTED_PROPERTIES;

  /** @deprecated Use {@link #LYO_NEXT_PAGE} instead. */
  @Deprecated String OSLC4J_NEXT_PAGE = LYO_NEXT_PAGE;

  /** @deprecated Use {@link #LYO_TOTAL_COUNT} instead. */
  @Deprecated String OSLC4J_TOTAL_COUNT = LYO_TOTAL_COUNT;

  /** @deprecated Use {@link #LYO_ALWAYS_XML_ABBREV} instead. */
  @Deprecated String OSLC4J_ALWAYS_XML_ABBREV = LYO_ALWAYS_XML_ABBREV;
}
