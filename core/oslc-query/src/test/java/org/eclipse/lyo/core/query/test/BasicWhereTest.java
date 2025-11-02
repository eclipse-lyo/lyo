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
package org.eclipse.lyo.core.query.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.Map;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.UriRefValue;
import org.eclipse.lyo.core.query.Value;
import org.eclipse.lyo.core.query.WhereClause;
import org.junit.Test;

/**
 * Basic tests of oslc.where clause parsing
 */
public class BasicWhereTest {
  static final String PREFIXES =
      "qm=<http://qm.example.com/ns#>,"
          + "olsc=<http://open-services.net/ns/core#>,"
          + "xs=<http://www.w3.org/2001/XMLSchema>";

  @Test
  public void testWhere() throws ParseException {

    String[] expressions = {
      "qm:testcase=<http://example.com/tests/31459>",
      "qm:duration>=10.4",
      "oslc:create!=\"Bob\" and qm:verified!=true",
      "qm:state in [\"Done\",\"Open\"]",
      "oslc:verified_by{oslc:owner=\"Steve\" and qm:duration=-47.0} and"
          + " oslc:description=\"very hairy expression\"",
      "qm:submitted<\"2011-10-10T07:00:00Z\"^^xs:dateTime",
      "oslc:label>\"The End\"@en-US"
    };

    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);

    for (String expression : expressions) {
      WhereClause whereClause = (WhereClause) QueryUtils.parseWhere(expression, prefixMap);

      System.out.println(whereClause);
    }
  }

  @Test(expected = ParseException.class)
  public void testBadWhere() throws ParseException {
    Map<String, String> prefixMap = null;
    try {
      prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    } catch (ParseException e) {
      fail("Couldn't parse prefixes");
    }

    QueryUtils.parseSelect("XXX", prefixMap);
  }

  @Test
  public void testUriRef() throws ParseException {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    WhereClause where =
        QueryUtils.parseWhere("qm:testCase=<http://example.org/tests/24>", prefixMap);

    List<SimpleTerm> children = where.children();
    assertEquals("Where clause should only have one term", 1, children.size());

    SimpleTerm simpleTerm = children.get(0);
    PName prop = simpleTerm.property();
    assertEquals(prop.namespace + prop.local, "http://qm.example.com/ns#testCase");
    assertTrue(simpleTerm instanceof ComparisonTerm);

    ComparisonTerm comparison = (ComparisonTerm) simpleTerm;
    assertEquals(comparison.operator(), ComparisonTerm.Operator.EQUALS);

    Value v = comparison.operand();
    assertTrue(v instanceof UriRefValue);

    UriRefValue uriRef = (UriRefValue) v;
    assertEquals("http://example.org/tests/24", uriRef.value());
  }

  // Helper method to reduce redundancy
  private ComparisonTerm parseWhereCondition(String expression, Map<String, String> prefixMap)
      throws ParseException {
    WhereClause where = QueryUtils.parseWhere(expression, prefixMap);
    List<SimpleTerm> children = where.children();
    assertEquals("Where clause should have one term for this test", 1, children.size());
    SimpleTerm simpleTerm = children.get(0);
    assertTrue("Term should be a ComparisonTerm", simpleTerm instanceof ComparisonTerm);
    return (ComparisonTerm) simpleTerm;
  }

  @Test
  public void testLessThanInteger() throws ParseException {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    ComparisonTerm comparison = parseWhereCondition("qm:answer<42", prefixMap);

    PName prop = comparison.property();
    assertEquals("http://qm.example.com/ns#answer", prop.namespace + prop.local);
    assertEquals(ComparisonTerm.Operator.LESS_THAN, comparison.operator());

    Value v = comparison.operand();
    assertTrue(
        "Operand should be a literal decimal value",
        v instanceof org.eclipse.lyo.core.query.DecimalValue);
    org.eclipse.lyo.core.query.DecimalValue decimalValue =
        (org.eclipse.lyo.core.query.DecimalValue) v;
    assertEquals(42.0, Double.parseDouble(decimalValue.value()), 0.001);
  }

  @Test
  public void testLessThanOrEqualsInteger() throws ParseException {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    ComparisonTerm comparison = parseWhereCondition("qm:answer<=42", prefixMap);

    PName prop = comparison.property();
    assertEquals("http://qm.example.com/ns#answer", prop.namespace + prop.local);
    assertEquals(ComparisonTerm.Operator.LESS_EQUALS, comparison.operator());

    Value v = comparison.operand();
    assertTrue(
        "Operand should be a literal decimal value",
        v instanceof org.eclipse.lyo.core.query.DecimalValue);
    org.eclipse.lyo.core.query.DecimalValue decimalValue =
        (org.eclipse.lyo.core.query.DecimalValue) v;
    assertEquals(42.0, Double.parseDouble(decimalValue.value()), 0.001);
  }

  @Test
  public void testLessThanOrEqualsString() throws ParseException {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    ComparisonTerm comparison = parseWhereCondition("qm:question<=\"The ultimate...\"", prefixMap);

    PName prop = comparison.property();
    assertEquals("http://qm.example.com/ns#question", prop.namespace + prop.local);
    assertEquals(ComparisonTerm.Operator.LESS_EQUALS, comparison.operator());

    Value v = comparison.operand();
    assertTrue(
        "Operand should be a literal string value",
        v instanceof org.eclipse.lyo.core.query.StringValue);
    org.eclipse.lyo.core.query.StringValue stringValue = (org.eclipse.lyo.core.query.StringValue) v;
    assertEquals("The ultimate...", stringValue.value());
  }

  @Test
  public void testLessThanOrEqualsUri() throws ParseException {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    ComparisonTerm comparison = parseWhereCondition("qm:question<=<urn:qm:ultimate>", prefixMap);

    PName prop = comparison.property();
    assertEquals("http://qm.example.com/ns#question", prop.namespace + prop.local);
    assertEquals(ComparisonTerm.Operator.LESS_EQUALS, comparison.operator());

    Value v = comparison.operand();
    assertTrue("Operand should be a UriRefValue", v instanceof UriRefValue);
    UriRefValue uriRefValue = (UriRefValue) v;
    assertEquals("urn:qm:ultimate", uriRefValue.value());
  }

  @Test
  public void testLessThanNegativeInteger() throws ParseException {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    ComparisonTerm comparison = parseWhereCondition("qm:age<-5", prefixMap);

    PName prop = comparison.property();
    assertEquals("http://qm.example.com/ns#age", prop.namespace + prop.local);
    assertEquals(ComparisonTerm.Operator.LESS_THAN, comparison.operator());

    Value v = comparison.operand();
    assertTrue(
        "Operand should be a literal decimal value",
        v instanceof org.eclipse.lyo.core.query.DecimalValue);
    org.eclipse.lyo.core.query.DecimalValue decimalValue =
        (org.eclipse.lyo.core.query.DecimalValue) v;
    assertEquals(-5.0, Double.parseDouble(decimalValue.value()), 0.001);
  }

  @Test
  public void testLessThanOrEqualsEmptyString() throws ParseException {
    Map<String, String> prefixMap = QueryUtils.parsePrefixes(PREFIXES);
    ComparisonTerm comparison = parseWhereCondition("qm:name<=\"\"", prefixMap);

    PName prop = comparison.property();
    assertEquals("http://qm.example.com/ns#name", prop.namespace + prop.local);
    assertEquals(ComparisonTerm.Operator.LESS_EQUALS, comparison.operator());

    Value v = comparison.operand();
    assertTrue(
        "Operand should be a literal string value",
        v instanceof org.eclipse.lyo.core.query.StringValue);
    org.eclipse.lyo.core.query.StringValue stringValue = (org.eclipse.lyo.core.query.StringValue) v;
    assertEquals("", stringValue.value());
  }
}
