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
import org.eclipse.lyo.core.query.BooleanValue;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.DecimalValue;
import org.eclipse.lyo.core.query.InTerm;
import org.eclipse.lyo.core.query.LangedStringValue;
import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.OrderByClause;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.ScopedSortTerm;
import org.eclipse.lyo.core.query.SelectClause;
import org.eclipse.lyo.core.query.SimpleSortTerm;
import org.eclipse.lyo.core.query.StringValue;
import org.eclipse.lyo.core.query.TypedValue;
import org.eclipse.lyo.core.query.UriRefValue;
import org.eclipse.lyo.core.query.WhereClause;
import org.junit.Test;

/** Regression coverage for the query forms required by the OSLC specifications. */
public class OslcQuerySpecificationTest {
  private static final String PREFIXES =
      "dcterms=<http://purl.org/dc/terms/>,"
          + "oslc=<http://open-services.net/ns/core#>,"
          + "rdf=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>,"
          + "xs=<http://www.w3.org/2001/XMLSchema>";

  @Test
  public void parseWhereBuildsTheSpecificationValueTypes() throws Exception {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    WhereClause where =
        QueryUtils.parseWhere(
            "dcterms:identifier=<https://example.test/requirements/1> and "
                + "dcterms:title=\"Requirement\" and "
                + "oslc:status in [\"Open\",\"Done\"] and "
                + "oslc:priority>=1.50 and "
                + "oslc:approved=true and "
                + "dcterms:modified=\"2026-09-19T00:00:00Z\"^^xs:dateTime and "
                + "dcterms:title=\"Anforderung\"@de-DE",
            prefixMap);

    assertEquals(7, where.children().size());

    ComparisonTerm uriComparison = (ComparisonTerm) where.children().get(0);
    assertEquals(ComparisonTerm.Operator.EQUALS, uriComparison.operator());
    assertTrue(uriComparison.operand() instanceof UriRefValue);
    assertEquals(
        "https://example.test/requirements/1", ((UriRefValue) uriComparison.operand()).value());

    ComparisonTerm stringComparison = (ComparisonTerm) where.children().get(1);
    assertTrue(stringComparison.operand() instanceof StringValue);
    assertEquals("Requirement", ((StringValue) stringComparison.operand()).value());

    InTerm inTerm = (InTerm) where.children().get(2);
    assertEquals(2, inTerm.values().size());
    assertEquals("Open", ((StringValue) inTerm.values().get(0)).value());
    assertEquals("Done", ((StringValue) inTerm.values().get(1)).value());

    ComparisonTerm decimalComparison = (ComparisonTerm) where.children().get(3);
    assertEquals(ComparisonTerm.Operator.GREATER_EQUALS, decimalComparison.operator());
    assertTrue(decimalComparison.operand() instanceof DecimalValue);
    assertEquals("1.50", ((DecimalValue) decimalComparison.operand()).value());

    ComparisonTerm booleanComparison = (ComparisonTerm) where.children().get(4);
    assertTrue(booleanComparison.operand() instanceof BooleanValue);
    assertTrue(((BooleanValue) booleanComparison.operand()).value());

    ComparisonTerm typedComparison = (ComparisonTerm) where.children().get(5);
    assertTrue(typedComparison.operand() instanceof TypedValue);
    TypedValue typedValue = (TypedValue) typedComparison.operand();
    assertEquals("2026-09-19T00:00:00Z", typedValue.value());
    assertEquals("http://www.w3.org/2001/XMLSchema", typedValue.prefixedName().namespace);
    assertEquals("dateTime", typedValue.prefixedName().local);

    ComparisonTerm languageComparison = (ComparisonTerm) where.children().get(6);
    assertTrue(languageComparison.operand() instanceof LangedStringValue);
    LangedStringValue languageValue = (LangedStringValue) languageComparison.operand();
    assertEquals("Anforderung", languageValue.value());
    assertEquals("de-DE", languageValue.langTag());
  }

  @Test
  public void parseWhereSupportsAllComparisonOperatorsAndEscapedStrings() throws Exception {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    WhereClause where =
        QueryUtils.parseWhere(
            "oslc:a=\"one\" and oslc:b!=\"two\" and oslc:c<\"three\" and "
                + "oslc:d>\"four\" and oslc:e<=\"five\" and oslc:f>=\"six\" and "
                + "dcterms:title=\"A \\\"quoted\\\" title\"",
            prefixMap);

    assertEquals(7, where.children().size());
    assertEquals(
        ComparisonTerm.Operator.EQUALS, ((ComparisonTerm) where.children().get(0)).operator());
    assertEquals(
        ComparisonTerm.Operator.NOT_EQUALS, ((ComparisonTerm) where.children().get(1)).operator());
    assertEquals(
        ComparisonTerm.Operator.LESS_THAN, ((ComparisonTerm) where.children().get(2)).operator());
    assertEquals(
        ComparisonTerm.Operator.GREATER_THAN,
        ((ComparisonTerm) where.children().get(3)).operator());
    assertEquals(
        ComparisonTerm.Operator.LESS_EQUALS, ((ComparisonTerm) where.children().get(4)).operator());
    assertEquals(
        ComparisonTerm.Operator.GREATER_EQUALS,
        ((ComparisonTerm) where.children().get(5)).operator());
    assertTrue(((ComparisonTerm) where.children().get(6)).operand() instanceof StringValue);
  }

  @Test
  public void parseWhereSupportsPrefixedUriValues() throws Exception {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    WhereClause where = QueryUtils.parseWhere("dcterms:relatedTo=oslc:Resource", prefixMap);

    UriRefValue uriValue = (UriRefValue) ((ComparisonTerm) where.children().get(0)).operand();
    assertEquals("http://open-services.net/ns/core#Resource", uriValue.value());
  }

  @Test
  public void parseSelectBuildsNestedPropertiesAndWildcards() throws Exception {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    SelectClause select =
        QueryUtils.parseSelect(
            "dcterms:title,oslc:shortTitle,oslc:relatedArtifact{"
                + "dcterms:title,oslc:identifier},*",
            prefixMap);

    assertEquals(4, select.children().size());
    assertEquals("title", select.children().get(0).identifier().local);
    assertEquals("shortTitle", select.children().get(1).identifier().local);
    assertTrue(select.children().get(2) instanceof NestedProperty);

    NestedProperty nested = (NestedProperty) select.children().get(2);
    assertEquals("relatedArtifact", nested.identifier().local);
    assertEquals(2, nested.children().size());
    assertTrue(select.children().get(3).isWildcard());

    Map<String, Object> inverted =
        QueryUtils.invertSelectedProperties(
            QueryUtils.parseSelect("dcterms:title,oslc:shortTitle", prefixMap));
    assertTrue(inverted.containsKey("http://purl.org/dc/terms/title"));
    assertTrue(inverted.containsKey("http://open-services.net/ns/core#shortTitle"));
  }

  @Test
  public void parseOrderByBuildsDirectionsAndScopedTerms() throws Exception {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    OrderByClause orderBy =
        QueryUtils.parseOrderBy(
            "-dcterms:title,+oslc:identifier,oslc:relatedArtifact{-dcterms:title}", prefixMap);

    assertEquals(3, orderBy.children().size());

    SimpleSortTerm descending = (SimpleSortTerm) orderBy.children().get(0);
    assertFalse(descending.ascending());
    assertEquals("title", descending.identifier().local);

    SimpleSortTerm ascending = (SimpleSortTerm) orderBy.children().get(1);
    assertTrue(ascending.ascending());
    assertEquals("identifier", ascending.identifier().local);

    assertTrue(orderBy.children().get(2) instanceof ScopedSortTerm);
    ScopedSortTerm scoped = (ScopedSortTerm) orderBy.children().get(2);
    assertEquals(1, scoped.sortTerms().children().size());
    assertFalse(((SimpleSortTerm) scoped.sortTerms().children().get(0)).ascending());
  }

  @Test
  public void parseSearchTermsPreservesTermsContainingSpacesAndEscapes() throws Exception {
    var searchTerms =
        QueryUtils.parseSearchTerms("\"Baseline A\",\"Active stream\",\"A \\\"quoted\\\" value\"");

    assertEquals(3, searchTerms.size());
    assertEquals("Baseline A", searchTerms.get(0));
    assertEquals("Active stream", searchTerms.get(1));
    assertEquals("A \\\"quoted\\\" value", searchTerms.get(2));
  }
}
