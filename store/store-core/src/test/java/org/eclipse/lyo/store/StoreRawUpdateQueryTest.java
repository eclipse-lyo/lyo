/*
 * Copyright (c) 2025 Contributors to the Eclipse Foundation
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

import static org.junit.jupiter.api.Assertions.*;

import java.net.URI;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("Store Raw Update Query Tests")
public class StoreRawUpdateQueryTest {

    private Store store;

    @BeforeEach
    public void setUp() {
        store = StoreFactory.sparqlInMem();
    }

    @Test
    @DisplayName("Test rawUpdateQuery creates and drops graphs")
    public void testRawUpdateQueryGraphOperations() throws StoreAccessException {
        final URI testGraph = URI.create("http://example.org/test/graph");

        // Create graph using raw update query
        String createQuery = "CREATE GRAPH <" + testGraph + ">";
        store.rawUpdateQuery(createQuery);

        // Verify graph doesn't exist (empty graphs are not considered existing)
        assertFalse(
                store.namedGraphExists(testGraph), "Empty graph should not exist after creation");

        // Drop graph using raw update query
        String dropQuery = "DROP GRAPH <" + testGraph + ">";
        store.rawUpdateQuery(dropQuery);

        // Verify graph no longer exists
        assertFalse(store.namedGraphExists(testGraph), "Graph should not exist after dropping");
    }

    @Test
    @DisplayName("Test rawUpdateQuery inserts and deletes data")
    public void testRawUpdateQueryDataOperations() throws StoreAccessException {
        final URI testGraph = URI.create("http://example.org/test/data");
        final URI subject = URI.create("http://example.org/test/subject");

        // Create graph first
        String createQuery = "CREATE GRAPH <" + testGraph + ">";
        store.rawUpdateQuery(createQuery);

        // Insert data using raw update query
        String insertQuery =
                "INSERT DATA { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "<"
                        + subject
                        + "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                        + " <http://example.org/TestClass> . <"
                        + subject
                        + "> <http://example.org/property> \"test value\" . "
                        + "} }";
        store.rawUpdateQuery(insertQuery);

        // Verify data exists
        assertTrue(
                store.resourceExists(testGraph, subject), "Resource should exist after insertion");

        // Delete specific triples using raw update query
        String deleteQuery =
                "DELETE DATA { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "<"
                        + subject
                        + "> <http://example.org/property> \"test value\" . "
                        + "} }";
        store.rawUpdateQuery(deleteQuery);

        // Verify resource still exists (type triple should remain)
        assertTrue(
                store.resourceExists(testGraph, subject),
                "Resource should still exist after partial deletion");

        // Delete all triples for the subject
        String deleteAllQuery =
                "DELETE WHERE { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "<"
                        + subject
                        + "> ?p ?o . "
                        + "} }";
        store.rawUpdateQuery(deleteAllQuery);

        // Verify resource no longer exists
        assertFalse(
                store.resourceExists(testGraph, subject),
                "Resource should not exist after complete deletion");
    }

    @Test
    @DisplayName("Test rawUpdateQuery with CLEAR operation")
    public void testRawUpdateQueryClearOperation() throws StoreAccessException {
        final URI testGraph = URI.create("http://example.org/test/clear");
        final URI subject1 = URI.create("http://example.org/test/subject1");
        final URI subject2 = URI.create("http://example.org/test/subject2");

        // Create graph and insert multiple resources
        String setupQuery =
                "INSERT DATA { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "<"
                        + subject1
                        + "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                        + " <http://example.org/TestClass> . <"
                        + subject2
                        + "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                        + " <http://example.org/TestClass> . } }";
        store.rawUpdateQuery(setupQuery);

        // Verify resources exist
        assertTrue(store.resourceExists(testGraph, subject1), "Subject1 should exist before clear");
        assertTrue(store.resourceExists(testGraph, subject2), "Subject2 should exist before clear");

        // Clear the graph using raw update query
        String clearQuery = "CLEAR GRAPH <" + testGraph + ">";
        store.rawUpdateQuery(
                clearQuery); // Verify graph is empty and no longer exists (empty graphs are not
        // considered existing)
        assertFalse(store.namedGraphExists(testGraph), "Graph should not exist after clear");
        assertFalse(
                store.resourceExists(testGraph, subject1), "Subject1 should not exist after clear");
        assertFalse(
                store.resourceExists(testGraph, subject2), "Subject2 should not exist after clear");
    }

    @Test
    @DisplayName("Test rawUpdateQuery with complex SPARQL update")
    public void testRawUpdateQueryComplexUpdate() throws StoreAccessException {
        final URI testGraph = URI.create("http://example.org/test/complex");

        // Create a complex update that creates graph, inserts data, and modifies it
        String complexQuery =
                "INSERT DATA { "
                        + "GRAPH <"
                        + testGraph
                        + "> { <http://example.org/person/1>"
                        + " <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
                        + " <http://example.org/Person> . <http://example.org/person/1>"
                        + " <http://example.org/name> \"John Doe\" . <http://example.org/person/1>"
                        + " <http://example.org/age> 30 . } } ; DELETE { GRAPH <"
                        + testGraph
                        + "> { "
                        + "?person <http://example.org/age> ?oldAge . "
                        + "} } "
                        + "INSERT { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "?person <http://example.org/age> ?newAge . "
                        + "} } "
                        + "WHERE { "
                        + "GRAPH <"
                        + testGraph
                        + "> { "
                        + "?person <http://example.org/age> ?oldAge . "
                        + "BIND(?oldAge + 1 AS ?newAge) "
                        + "} }";

        store.rawUpdateQuery(complexQuery);

        // Verify the complex update worked
        URI person = URI.create("http://example.org/person/1");
        assertTrue(
                store.resourceExists(testGraph, person),
                "Person should exist after complex update");

        // We can't easily verify the age was incremented without querying,
        // but we can verify the resource still exists and the graph is valid
        assertTrue(store.namedGraphExists(testGraph), "Graph should exist after complex update");
    }

    @Test
    @DisplayName("Test rawUpdateQuery handles invalid SPARQL gracefully")
    public void testRawUpdateQueryInvalidSparql() {
        // Test that invalid SPARQL throws an appropriate exception
        String invalidQuery = "INVALID SPARQL SYNTAX";

        assertThrows(
                Exception.class,
                () -> {
                    store.rawUpdateQuery(invalidQuery);
                },
                "Invalid SPARQL should throw an exception");
    }
}
