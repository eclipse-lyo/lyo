package org.eclipse.lyo.store;

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

import org.apache.jena.tdb.TDBFactory;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.eclipse.lyo.store.internals.query.DatasetQueryExecutorImpl;

/**
 * Provides factory methods to instantiate concrete implementations of {@link Store} that keep the
 * triplestore information in various ways (RAM, disk, over the network).
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.16.0
 */
public class StoreFactory {

    /**
     * Initialise a SPARQL-compatible Store implementation that does not require authentication.
     *
     * @param queryUrl  SPARQL Query endpoint URI
     * @param updateUrl SPARQL Update endpoint URI
     * @return Store implementation that communicates with the triplestore via SPARQL.
     *
     */
    public static Store sparql(final String queryUrl, final String updateUrl) {
        return new SparqlStoreImpl(queryUrl, updateUrl);
    }

    public static Store sparqlInMem() {
        return new SparqlStoreImpl(new DatasetQueryExecutorImpl(TDBFactory.createDataset()));
    }

    /**
     * Initialise a SPARQL-compatible Store implementation with authentication via username and
     * password
     * combinations. Authentication works with the basic and digest HTTP authentication schemes.
     *
     * @param queryUrl  SPARQL Query endpoint URI
     * @param updateUrl SPARQL Update endpoint URI
     * @param username  Username
     * @param password  Password
     * @return Store implementation that communicates with the triplestore via SPARQL.
     *
     */
    public static Store sparql(final String queryUrl, final String updateUrl, final String username,
            final String password) {
        return new SparqlStoreImpl(queryUrl, updateUrl, username, password);
    }

}
