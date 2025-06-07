package org.eclipse.lyo.store.internals.query;

/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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

import org.apache.jena.query.QueryExecution;
import org.apache.jena.update.Update;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;

/**
 * QueryExecutor is an interface that allows to run SPARQL queries on different triplestore
 * interfaces (network endpoints, Jena API, etc.).
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public interface JenaQueryExecutor {

    /**
     * Prepares a SPARQL Query executor (read-only).
     *
     * @param query SPARQL query string
     * @return prepared executor
     */
    QueryExecution prepareSparqlQuery(String query);

    /**
     * Prepares a SPARQL Update processor (write-only).
     *
     * @return prepared processor
     */
    UpdateProcessor prepareSparqlUpdate(final UpdateRequest updateRequest);

    /**
     * Prepares a SPARQL Update processor (write-only).
     *
     * @return prepared processor
     */
    UpdateProcessor prepareSparqlUpdate(final Update update);

    /**
     * Prepares a SPARQL Update processor (write-only).
     *
     * @param query SPARQL query string
     * @return prepared processor
     */
    UpdateProcessor prepareSparqlUpdate(String query);

    /**
     * Release a connection to the underlying engine
     */
    void release();

    void beginWrite();

    void beginRead();

    void commit();

    void end();
}
