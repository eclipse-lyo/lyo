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
package org.eclipse.lyo.oslc4j.core.model;

import jakarta.ws.rs.core.MediaType;

/**
 * Constant strings and static MediaType representing OSLC media types
 * @see jakarta.ws.rs.core.MediaType
 */
public interface OslcMediaType {

  public static final String APPLICATION = "application";
  public static final String TEXT = "text";

  public static final String RDF_XML = "rdf+xml";
  public static final String APPLICATION_RDF_XML = APPLICATION + "/" + RDF_XML;
  public static final MediaType APPLICATION_RDF_XML_TYPE = new MediaType(APPLICATION, RDF_XML);

  public static final String JSON_LD = "ld+json";
  public static final String APPLICATION_JSON_LD = APPLICATION + "/" + JSON_LD;
  public static final MediaType APPLICATION_JSON_LD_TYPE = new MediaType(APPLICATION, JSON_LD);

  public static final String APPLICATION_JSON = MediaType.APPLICATION_JSON;
  public static final MediaType APPLICATION_JSON_TYPE = MediaType.APPLICATION_JSON_TYPE;

  public static final String APPLICATION_XML = MediaType.APPLICATION_XML;
  public static final MediaType APPLICATION_XML_TYPE = MediaType.APPLICATION_XML_TYPE;

  public static final String TEXT_XML = MediaType.TEXT_XML;
  public static final MediaType TEXT_XML_TYPE = MediaType.TEXT_XML_TYPE;

  public static final String TURTLE = "turtle";
  public static final String TEXT_TURTLE = TEXT + "/" + TURTLE;
  public static final MediaType TEXT_TURTLE_TYPE = new MediaType(TEXT, TURTLE);

  public static final String X_OSLC_COMPACT_XML = "x-oslc-compact+xml";
  public static final String APPLICATION_X_OSLC_COMPACT_XML =
      APPLICATION + "/" + X_OSLC_COMPACT_XML;
  public static final MediaType APPLICATION_X_OSLC_COMPACT_XML_TYPE =
      new MediaType(APPLICATION, X_OSLC_COMPACT_XML);

  public static final String X_OSLC_COMPACT_JSON =
      "x-oslc-compact+json"; // TODO - Compact media type never defined in the OSLC spec for JSON
  public static final String APPLICATION_X_OSLC_COMPACT_JSON =
      APPLICATION + "/" + X_OSLC_COMPACT_JSON;
  public static final MediaType APPLICATION_X_OSLC_COMPACT_JSON_TYPE =
      new MediaType(APPLICATION, X_OSLC_COMPACT_JSON);
}
