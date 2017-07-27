package org.eclipse.lyo.store.internals;

/*-
 * #%L
 * Contributors:
 *     Andrew Berezovskyi - initial implementation
 * %%
 * Copyright (C) 2016 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

import com.hp.hpl.jena.query.ARQ;
import com.hp.hpl.jena.query.Dataset;
import com.hp.hpl.jena.tdb.TDB;
import com.hp.hpl.jena.tdb.TDBFactory;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
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
