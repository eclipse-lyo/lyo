package org.eclipse.lyo.store;

/*-
 * #%L
 * Contributors:
 *     Andrew Berezovskyi - initial implementation
 *     Jad El-khoury - basic auth method
 * %%
 * Copyright (C) 2016 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

import org.apache.jena.query.Dataset;
import org.apache.jena.tdb.TDBFactory;
import java.io.IOException;
import java.nio.file.Path;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.eclipse.lyo.store.internals.DatasetBuilder;
import org.eclipse.lyo.store.internals.JenaTdbStoreImpl;

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
     * Initialise an in-memory Store implementation.
     * <p>
     * <p>Supports all SPARQL queries and TDB transactions.</p>
     *
     * @return Store implementation that keeps triples in memory.
     */
    @Deprecated
    public static Store inMemory() {
        final Dataset dataset = TDBFactory.createDataset();
        return new JenaTdbStoreImpl(dataset);
    }

    /**
     * Initialise an on-disk Store implementation.
     * <p>
     * <p>Tries to open a triplestore under a given path and then initialise one if it doesn't
     * exist. Will create the necessary directories.</p>
     *
     * @param path Filesystem path where the Jena TDB files can be stored.
     * @return Store implementation that keeps triples on disk.
     *
     * @throws IllegalArgumentException If the TDB cannot be initialised in a given directory.
     */
    @Deprecated
    public static Store onDisk(final Path path) throws IllegalArgumentException {
        try {
            final Dataset dataset = DatasetBuilder.buildPersistent(path);
            return new JenaTdbStoreImpl(dataset);
        } catch (final IOException e) {
            throw new IllegalArgumentException("Cannot initialise TDB under a given path", e);
        }
    }

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
