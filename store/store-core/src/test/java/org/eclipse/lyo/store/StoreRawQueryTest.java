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

package org.eclipse.lyo.store;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.RDFS;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Store Raw Query Tests")
public class StoreRawQueryTest {

    private static final String RDF_TYPE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

    private Store store;

    @BeforeEach
    public void setUp() {
        store = StoreFactory.sparqlInMem();
    }

    @Test
    @DisplayName("Test rawQuery executes a CONSTRUCT query against a named graph")
    public void testRawQueryConstruct() throws StoreAccessException {
        final URI testGraph = URI.create("http://example.org/test/query/graph");
        final URI subject = URI.create("http://example.org/test/query/subject");

        final String insertQuery =
                "INSERT DATA { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "<"
                        + subject
                        + "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                        + " <http://example.org/Person> . <"
                        + subject
                        + "> <http://example.org/name> \"John Doe\" . "
                        + "} }";
        store.rawUpdateQuery(insertQuery);

        final String constructQuery =
                "CONSTRUCT { ?s ?p ?o } WHERE { "
                        + "GRAPH <"
                        + testGraph
                        + "> { ?s ?p ?o } "
                        + "}";
        final Model result = store.rawQuery(constructQuery);

        assertEquals(2, result.size(), "Model should contain exactly the two inserted triples");
        assertTrue(
                result.contains(
                        ResourceFactory.createResource(subject.toString()),
                        ResourceFactory.createProperty("http://example.org/name"),
                        ResourceFactory.createTypedLiteral("John Doe")),
                "Model should contain the inserted name statement");
        assertTrue(
                result.contains(
                        ResourceFactory.createResource(subject.toString()),
                        ResourceFactory.createProperty(RDF_TYPE),
                        ResourceFactory.createResource("http://example.org/Person")),
                "Model should contain the inserted type statement");
    }

    @Test
    @DisplayName("Test rawQuery executes a DESCRIBE query")
    public void testRawQueryDescribe() throws StoreAccessException {
        final URI subject = URI.create("http://example.org/test/query/describe-subject");

        final String insertQuery =
                "INSERT DATA { "
                        + "<"
                        + subject
                        + "> <http://example.org/name> \"Jane Doe\" . "
                        + "}";
        store.rawUpdateQuery(insertQuery);

        final String describeQuery = "DESCRIBE <" + subject + "> WHERE {}";
        final Model result = store.rawQuery(describeQuery);

        assertEquals(1, result.size(), "Model should contain the description statements");
        assertTrue(
                result.contains(
                        ResourceFactory.createResource(subject.toString()),
                        ResourceFactory.createProperty("http://example.org/name"),
                        ResourceFactory.createTypedLiteral("Jane Doe")),
                "Model should contain the name statement of the described resource");
    }

    @Test
    @DisplayName("Test rawQuery reads data written via the Store API")
    public void testRawQueryReadsStoreApiData() throws StoreAccessException {
        final URI testGraph = URI.create("http://example.org/test/query/api-graph");
        final URI subject = URI.create("http://example.org/test/query/api-subject");

        final Model model = ModelFactory.createDefaultModel();
        model
                .createResource(subject.toString())
                .addProperty(RDFS.label, "written via the Store API");
        store.insertJenaModel(testGraph, model);

        final String constructQuery =
                "CONSTRUCT { ?s ?p ?o } WHERE { "
                        + "GRAPH <"
                        + testGraph
                        + "> { ?s ?p ?o } "
                        + "}";
        final Model result = store.rawQuery(constructQuery);

        assertTrue(
                result.contains(
                        ResourceFactory.createResource(subject.toString()),
                        RDFS.label,
                        "written via the Store API"),
                "Model should contain the statement inserted via insertJenaModel");
    }

    @Test
    @DisplayName("Test rawQuery returns an empty model when no triples match")
    public void testRawQueryEmptyResult() throws StoreAccessException {
        final Model result = store.rawQuery("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }");

        assertEquals(0, result.size(), "Model should be empty when no triples match");
    }

    @Test
    @DisplayName("Test rawSelectQuery executes a SELECT query against a named graph")
    public void testRawSelectQuery() throws StoreAccessException {
        final URI testGraph = URI.create("http://example.org/test/query/select-graph");
        final URI subject = URI.create("http://example.org/test/query/select-subject");

        final String insertQuery =
                "INSERT DATA { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "<"
                        + subject
                        + "> <http://example.org/name> \"John Doe\" . "
                        + "} }";
        store.rawUpdateQuery(insertQuery);

        final String selectQuery =
                "SELECT ?s ?name WHERE { "
                        + "GRAPH <"
                        + testGraph
                        + "> { ?s <http://example.org/name> ?name } "
                        + "}";
        final ResultSet result = store.rawSelectQuery(selectQuery);
        final ResultSetRewindable rewindableResult = result.rewindable();

        assertTrue(rewindableResult.hasNext(), "ResultSet should contain at least one solution");
        assertEquals(
                ResourceFactory.createResource(subject.toString()),
                rewindableResult.next().get("s"));
        assertEquals(1, rewindableResult.size(), "ResultSet should contain exactly one solution");

        // the ResultSet is copied into memory, so it can be iterated once again
        rewindableResult.reset();
        assertEquals("John Doe", rewindableResult.next().getLiteral("name").getString());
    }

    @Test
    @DisplayName("Test rawSelectQuery returns no solutions when nothing matches")
    public void testRawSelectQueryEmptyResult() throws StoreAccessException {
        final ResultSet result = store.rawSelectQuery("SELECT ?s WHERE { ?s ?p ?o }");

        assertFalse(result.hasNext(), "ResultSet should contain no solutions");
    }

    @Test
    @DisplayName("Test rawAskQuery returns the ASK result")
    public void testRawAskQuery() throws StoreAccessException {
        final URI subject = URI.create("http://example.org/test/query/ask-subject");

        final String insertQuery =
                "INSERT DATA { "
                        + "<"
                        + subject
                        + "> <http://example.org/name> \"Jane Doe\" . "
                        + "}";
        store.rawUpdateQuery(insertQuery);

        assertTrue(
                store.rawAskQuery("ASK { <" + subject + "> <http://example.org/name> ?name }"),
                "ASK query should return true for an existing statement");
        assertFalse(
                store.rawAskQuery("ASK { <http://example.org/no/such/uri> <http://example.org/name> ?name }"),
                "ASK query should return false for a missing statement");
    }

    @Test
    @DisplayName("Test raw query methods reject queries of a different type")
    public void testRawQueryRejectsOtherQueryTypes() {
        final String selectQuery = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
        final String askQuery = "ASK { ?s ?p ?o }";
        final String constructQuery = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";

        assertThrows(
                StoreAccessException.class,
                () -> store.rawQuery(selectQuery),
                "rawQuery should reject a SELECT query");
        assertThrows(
                StoreAccessException.class,
                () -> store.rawQuery(askQuery),
                "rawQuery should reject an ASK query");
        assertThrows(
                StoreAccessException.class,
                () -> store.rawSelectQuery(constructQuery),
                "rawSelectQuery should reject a CONSTRUCT query");
        assertThrows(
                StoreAccessException.class,
                () -> store.rawSelectQuery(askQuery),
                "rawSelectQuery should reject an ASK query");
        assertThrows(
                StoreAccessException.class,
                () -> store.rawAskQuery(selectQuery),
                "rawAskQuery should reject a SELECT query");
        assertThrows(
                StoreAccessException.class,
                () -> store.rawAskQuery(constructQuery),
                "rawAskQuery should reject a CONSTRUCT query");
    }

    @Test
    @DisplayName("Test raw query methods reject invalid SPARQL")
    public void testRawQueryRejectsInvalidSparql() {
        final String invalidQuery = "INVALID SPARQL SYNTAX";

        assertThrows(
                StoreAccessException.class,
                () -> store.rawQuery(invalidQuery),
                "rawQuery should reject invalid SPARQL");
        assertThrows(
                StoreAccessException.class,
                () -> store.rawSelectQuery(invalidQuery),
                "rawSelectQuery should reject invalid SPARQL");
        assertThrows(
                StoreAccessException.class,
                () -> store.rawAskQuery(invalidQuery),
                "rawAskQuery should reject invalid SPARQL");
    }
}
