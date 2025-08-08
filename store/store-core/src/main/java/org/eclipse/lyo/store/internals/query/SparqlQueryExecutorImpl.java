package org.eclipse.lyo.store.internals.query;

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

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.update.Update;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * SparqlQueryExecutorImpl is a SPARQL endpoint-based implementation of {@link JenaQueryExecutor}.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public class SparqlQueryExecutorImpl implements JenaQueryExecutor {
    private final Logger log = LoggerFactory.getLogger(SparqlQueryExecutorImpl.class);

    private final String queryEndpoint;
    private final String updateEndpoint;

    public SparqlQueryExecutorImpl(final String sparqlEndpoint, final String updateEndpoint) {
        this.queryEndpoint = sparqlEndpoint;
        this.updateEndpoint = updateEndpoint;
    }

    @Override
    public QueryExecution prepareSparqlQuery(final String query) {
        return QueryExecution.service(queryEndpoint, query);
    }


    @Override
    public UpdateProcessor prepareSparqlUpdate(final UpdateRequest updateRequest) {
        return UpdateExecutionFactory.createRemote(updateRequest, updateEndpoint);
    }

    @Override
    public UpdateProcessor prepareSparqlUpdate(final Update update) {
        return prepareSparqlUpdate(new UpdateRequest(update));
    }

    @Override
    public UpdateProcessor prepareSparqlUpdate(final String query) {
        return prepareSparqlUpdate(UpdateFactory.create(query));
    }

    @Override
    public void release() {
        log.trace("NOP, there is nothing to release");
    }

    @Override
    public void beginWrite() {
        return;
    }

    @Override
    public void beginRead() {
        return;
    }

    @Override
    public void commit() {
        return;
    }

    @Override
    public void end() {
        return;
    }
}
