/*
 * Copyright (c) 2023 Contributors to the Eclipse Foundation
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

import java.io.StringReader;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.impl.ReifierStd;
import org.apache.jena.riot.Lang;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.junit.jupiter.api.Test;

public class ReifierStdTest {
    @Test
    void simpleReificationTest() {
        final var ex = "https://www.w3.org/TR/rdf11-mt/#";
        final var reifiedTtl =
                """
                @prefix ex: <https://www.w3.org/TR/rdf11-mt/#> .
                @prefix dcterms: <http://purl.org/dc/terms/> .
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                ex:a ex:b ex:c .

                ex:graph1 rdf:type rdf:Statement .
                ex:graph1 rdf:subject ex:a .
                ex:graph1 rdf:predicate ex:b .
                ex:graph1 rdf:object ex:c .
                ex:graph1 dcterms:date "2023-08-21"^^xsd:date .
                """;

        Model model = ModelFactory.createDefaultModel();
        model.read(new StringReader(reifiedTtl), null, Lang.TURTLE.getName());
        var ttlGraph = model.getGraph();

        Triple matchTriple =
                Triple.create(
                        model.createResource(ex + "a").asNode(),
                        model.createResource(ex + "b").asNode(),
                        model.createResource(ex + "c").asNode());
        ExtendedIterator<Node> iter = ReifierStd.allNodes(ttlGraph, matchTriple);

        while (iter.hasNext()) {
            var t = iter.next();
            System.err.println(t);
            ExtendedIterator<Triple> iterReif = ttlGraph.find(t, null, null);
            while (iterReif.hasNext()) {
                var tt = iterReif.next();
                System.err.println("->" + tt);
            }
        }
    }
}
