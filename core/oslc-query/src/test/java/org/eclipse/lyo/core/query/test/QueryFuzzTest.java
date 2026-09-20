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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.code_intelligence.jazzer.junit.FuzzTest;
import com.code_intelligence.jazzer.mutation.annotation.NotNull;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.CompoundTerm;
import org.eclipse.lyo.core.query.InTerm;
import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.OrderByClause;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.Properties;
import org.eclipse.lyo.core.query.Property;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.ScopedSortTerm;
import org.eclipse.lyo.core.query.SelectClause;
import org.eclipse.lyo.core.query.SimpleSortTerm;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.SortTerm;
import org.eclipse.lyo.core.query.SortTerms;
import org.eclipse.lyo.core.query.TypedValue;
import org.eclipse.lyo.core.query.Value;
import org.eclipse.lyo.core.query.WhereClause;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/** Coverage-guided fuzzing for all public OSLC query parsing entry points. */
public class QueryFuzzTest {
  private static final Map<String, String> PREFIXES =
      Map.of(
          "dcterms", "http://purl.org/dc/terms/",
          "oslc", "http://open-services.net/ns/core#",
          "oslc_config", "http://open-services.net/ns/config#",
          "qm", "http://qm.example.com/ns/",
          "rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
          "xs", "http://www.w3.org/2001/XMLSchema");

  private static final String DUPLICATE_PREFIXES =
      "qm=<http://qm.example.com/ns/>,qm=<http://qm.example.com/other/>";
  private static final String TRAILING_PREFIX_INPUT =
      "qm=<http://qmm.example.com/ns/q,>m=<.example.com/ns/q,>m="
          + "<http://qm.example.com/other/>";
  private static final String DUPLICATE_NESTED_PROPERTIES =
      "qm:property{dcterms:title},qm:property{oslc:shortTitle}";

  @MethodSource("seedInputs")
  @FuzzTest(maxDuration = "90s")
  void fuzzQueryParsers(@NotNull String expression) throws Exception {
    runParser(() -> fuzzPrefixes(expression));
    runParser(() -> fuzzWhere(expression));
    runParser(() -> fuzzSelect(expression));
    runParser(() -> fuzzProperties(expression));
    runParser(() -> fuzzOrderBy(expression));
    runParser(() -> fuzzSearchTerms(expression));
  }

  static Stream<Arguments> seedInputs() {
    return Stream.of(
        "oslc=<http://open-services.net/ns/core#>,qm=<http://qm.example.com/ns/>",
        DUPLICATE_PREFIXES,
        TRAILING_PREFIX_INPUT,
        "dcterms:relatedTo=dcterms:title",
        "rdf:type=<http://open-services.net/ns/config#VersionResource> and "
            + "dcterms:title in [\"Baseline A\",\"Baseline B\",\"Active stream\"]",
        "oslc:status=\"Open\" and qm:priority>=1",
        "dcterms:title,oslc:shortTitle,qm:owner{dcterms:title}",
        "dcterms:title,oslc:relatedArtifact{dcterms:title,oslc:identifier},*",
        DUPLICATE_NESTED_PROPERTIES,
        "qm:property,*{dcterms:title}",
        "-dcterms:title,+oslc:identifier,oslc:relatedArtifact{-dcterms:title}",
        "\"Open\",\"In Progress\",\"Done\"",
        "\"Baseline A\",\"Active stream\",\"A \\\"quoted\\\" value\"",
        "unknown:property")
        .map(Arguments::arguments);
  }

  private static void fuzzPrefixes(String expression) throws ParseException {
    Map<String, String> parsed = QueryUtils.parsePrefixes(expression);
    if (hasDuplicatePrefix(expression)) {
      throw new AssertionError("duplicate prefix was accepted: " + expression);
    }
    assertNotNull(parsed);
  }

  private static void fuzzWhere(String expression) throws ParseException {
    WhereClause where = QueryUtils.parseWhere(expression, PREFIXES);
    visitTerms(where.children());
  }

  private static void fuzzSelect(String expression) throws ParseException {
    SelectClause select = QueryUtils.parseSelect(expression, PREFIXES);
    visitProperties(select);
    QueryUtils.invertSelectedProperties(select);
  }

  private static void fuzzProperties(String expression) throws ParseException {
    Properties properties = QueryUtils.parseProperties(expression, PREFIXES);
    visitProperties(properties);
  }

  private static void fuzzOrderBy(String expression) throws ParseException {
    OrderByClause orderBy = QueryUtils.parseOrderBy(expression, PREFIXES);
    visitSortTerms(orderBy);
  }

  private static void fuzzSearchTerms(String expression) throws ParseException {
    assertNotNull(QueryUtils.parseSearchTerms(expression));
  }

  private static void runParser(Parser parser) throws ParseException {
    try {
      parser.parse();
    } catch (ParseException expected) {
      // Invalid syntax is expected for most generated inputs.
    }
  }

  private static void visitTerms(Iterable<? extends SimpleTerm> terms) {
    for (SimpleTerm term : terms) {
      if (term instanceof CompoundTerm) {
        if (term.property() != null) {
          assertResolved(term.property());
        }
        visitTerms(((CompoundTerm) term).children());
      } else {
        if (term.property() != null) {
          assertResolved(term.property());
        }
        if (term instanceof ComparisonTerm) {
          assertResolvedValue(((ComparisonTerm) term).operand());
        } else if (term instanceof InTerm) {
          for (Value value : ((InTerm) term).values()) {
            assertResolvedValue(value);
          }
        }
      }
    }
  }

  private static void assertResolvedValue(Value value) {
    if (value instanceof TypedValue) {
      assertResolved(((TypedValue) value).prefixedName());
    }
  }

  private static void visitProperties(Properties properties) {
    Map<String, Object> inverted = QueryUtils.invertSelectedProperties(properties);
    visitProperties(properties, inverted);
  }

  private static void visitProperties(Properties properties, Map<String, Object> inverted) {
    for (Property property : properties.children()) {
      if (property.isWildcard()) {
        continue;
      }

      PName identifier = property.identifier();
      assertResolved(identifier);
      String propertyName = identifier.namespace + identifier.local;
      assertTrue("inverted property missing: " + propertyName, inverted.containsKey(propertyName));

      if (property instanceof NestedProperty) {
        Object nested = inverted.get(propertyName);
        assertTrue("nested property was not inverted: " + propertyName, nested instanceof Map);
        visitProperties((NestedProperty) property, castMap(nested));
      }
    }
  }

  private static void visitSortTerms(SortTerms sortTerms) {
    for (SortTerm sortTerm : sortTerms.children()) {
      assertResolved(sortTerm.identifier());
      if (sortTerm instanceof SimpleSortTerm) {
        ((SimpleSortTerm) sortTerm).ascending();
      } else if (sortTerm instanceof ScopedSortTerm) {
        visitSortTerms(((ScopedSortTerm) sortTerm).sortTerms());
      }
    }
  }

  @SuppressWarnings("unchecked")
  private static Map<String, Object> castMap(Object value) {
    return (Map<String, Object>) value;
  }

  private static void assertResolved(PName name) {
    assertNotNull("unresolved prefix: " + name.prefix, name.namespace);
  }

  private static boolean hasDuplicatePrefix(String expression) {
    Set<String> prefixes = new HashSet<>();
    int definitionStart = 0;
    boolean insideIri = false;

    for (int index = 0; index <= expression.length(); index++) {
      if (index < expression.length()) {
        char character = expression.charAt(index);
        if (character == '<') {
          insideIri = true;
        } else if (character == '>') {
          insideIri = false;
        }
      }

      if (index != expression.length()
          && (expression.charAt(index) != ',' || insideIri)) {
        continue;
      }

      String definition = expression.substring(definitionStart, index);
      int equals = definition.indexOf('=');
      if (equals > 0) {
        String prefix = definition.substring(0, equals).trim();
        if (!prefix.isEmpty() && !prefixes.add(prefix)) {
          return true;
        }
      }
      definitionStart = index + 1;
    }

    return false;
  }

  @FunctionalInterface
  private interface Parser {
    void parse() throws ParseException;
  }
}
