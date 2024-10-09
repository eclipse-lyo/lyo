package org.eclipse.lyo.store.internals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

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

import org.apache.jena.query.ARQ;
import org.apache.jena.query.Dataset;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Contains factory methods to initialise a Jena TDB Dataset instance.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
@SuppressWarnings("WeakerAccess")
public class DatasetBuilder {
    private final static Logger LOGGER = LoggerFactory.getLogger(DatasetBuilder.class);

    /**
     * Initialises a persistent Jena TDB Dataset instance in a given subdirectory (created if
     * doesn't exist).
     *
     * @return An instance of a TDB Dataset
     *
     * @throws IOException if a Dataset can't be initialised in the given
     *                     directory (check write permissions).
     */
    public static Dataset buildPersistent(final Path path) throws IOException {
        try {
            Files.createDirectories(path);
            LOGGER.info("Path {} should exists now", path);
            Dataset dataset = TDBFactory.createDataset(path.toString());
            // see https://jena.apache.org/documentation/tdb/datasets.html
            // (http://archive.is/2F7EA)
            // TODO per dataset only?
            ARQ.getContext().set(TDB.symUnionDefaultGraph, true);
            return dataset;
        } catch (final IOException e) {
            LOGGER.error(String.valueOf(e));
            throw e;
        }
    }
}
