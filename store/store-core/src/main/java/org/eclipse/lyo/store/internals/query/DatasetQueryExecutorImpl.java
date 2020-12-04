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

import org.apache.jena.query.Dataset;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.update.GraphStore;
import org.apache.jena.update.GraphStoreFactory;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;
import org.eclipse.lyo.store.StoreFactory;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * DatasetQueryExecutorImpl is a work-in-progress implementation of in-memory and on-disk store
 * implementation via {@link SparqlStoreImpl}.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public class DatasetQueryExecutorImpl implements JenaQueryExecutor {
    private static final Logger log = LoggerFactory.getLogger(DatasetQueryExecutorImpl.class);
    private final Dataset dataset;
    private final GraphStore graphStore;

    /**
     * Use {@link StoreFactory} instead.
     */
    public DatasetQueryExecutorImpl() {
        this(TDBFactory.createDataset());
    }

    DatasetQueryExecutorImpl(final Dataset dataset) {
        this.dataset = dataset;
        this.graphStore = GraphStoreFactory.create(dataset);
    }

    @Override
    public QueryExecution prepareSparqlQuery(final String query) {
        log.debug("Running query: '{}'", query);
        return QueryExecutionFactory.create(query, dataset);
    }

    @Override
    public UpdateProcessor prepareSparqlUpdate(final String query) {
        log.debug("Running update: '{}'", query);
        final UpdateRequest update = UpdateFactory.create(query);
        return UpdateExecutionFactory.create(update, graphStore);
    }
}
