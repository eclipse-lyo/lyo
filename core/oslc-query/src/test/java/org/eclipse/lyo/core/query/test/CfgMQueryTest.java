/*
 * Copyright (c) 2026 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.core.query.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Map;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.InTerm;
import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.OrderByClause;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SelectClause;
import org.eclipse.lyo.core.query.SimpleSortTerm;
import org.eclipse.lyo.core.query.UriRefValue;
import org.eclipse.lyo.core.query.WhereClause;
import org.junit.Test;

/** Regression coverage for OSLC Configuration Management query shapes. */
public class CfgMQueryTest {
  private static final String PREFIXES =
      "dcterms=<http://purl.org/dc/terms/>,"
          + "oslc_config=<http://open-services.net/ns/config#>,"
          + "rdf=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>";

  @Test
  public void parseConfigurationResourceQuery() throws Exception {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    WhereClause where =
        QueryUtils.parseWhere(
            "rdf:type=<http://open-services.net/ns/config#VersionResource> and "
                + "dcterms:title in [\"Baseline A\",\"Baseline B\",\"Active stream\"]",
            prefixMap);
    SelectClause select =
        QueryUtils.parseSelect(
            "dcterms:title,oslc_config:component{dcterms:title}," + "oslc_config:versionResource",
            prefixMap);
    OrderByClause orderBy =
        QueryUtils.parseOrderBy("-dcterms:title,+oslc_config:versionResource", prefixMap);

    assertEquals(2, where.children().size());
    assertTrue(((ComparisonTerm) where.children().get(0)).operand() instanceof UriRefValue);
    assertEquals(
        "http://open-services.net/ns/config#VersionResource",
        ((UriRefValue) ((ComparisonTerm) where.children().get(0)).operand()).value());
    assertEquals(3, ((InTerm) where.children().get(1)).values().size());

    assertEquals(3, select.children().size());
    assertTrue(select.children().get(1) instanceof NestedProperty);
    assertEquals(1, ((NestedProperty) select.children().get(1)).children().size());

    assertEquals(2, orderBy.children().size());
    assertFalse(((SimpleSortTerm) orderBy.children().get(0)).ascending());
    assertTrue(((SimpleSortTerm) orderBy.children().get(1)).ascending());
  }

  @Test
  public void parseConfigurationHistorySearchTerms() throws Exception {
    var searchTerms =
        QueryUtils.parseSearchTerms("\"Baseline A\",\"Baseline B\",\"Active stream\"");

    assertEquals(3, searchTerms.size());
    org.junit.Assert.assertTrue(searchTerms.contains("Baseline A"));
    org.junit.Assert.assertTrue(searchTerms.contains("Baseline B"));
    org.junit.Assert.assertTrue(searchTerms.contains("Active stream"));
  }
}
