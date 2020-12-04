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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

import org.apache.jena.query.Dataset;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import org.assertj.core.api.Assertions;
import org.eclipse.lyo.store.internals.DatasetBuilder;
import org.junit.Ignore;
import org.junit.Test;

/**
 * DatasetBuilderTest is .
 * @author Andrew Berezovskyi <andriib@kth.se>
 * @since 2016-11-01
 */
@SuppressWarnings("PMD.LongVariable")
public class DatasetBuilderTest {

    private static final String DATASET_NAME = "testName";
    private static final String PATH_PREFIX = "jenaTest";
    private Dataset dataset;

    @Test
    public void testNewDatasetIsEmpty() throws IOException {
        final Path tempDirectory = Files.createTempDirectory(DatasetBuilderTest.PATH_PREFIX);
        dataset = DatasetBuilder.buildPersistent(tempDirectory);
        final Iterator<String> names = dataset.listNames();

        Assertions.assertThat(names).isEmpty();
    }


    @Ignore("Because of Gitlab CI container permissions")
    @Test(expected = IOException.class)
    public void testWrongPathCausesExceptionNoPermissions() throws IOException {
        final Path tempDirectory = Paths.get("/root/test");
        dataset = DatasetBuilder.buildPersistent(tempDirectory);
        final Iterator<String> names = dataset.listNames();

        Assertions.assertThat(names).isEmpty();
    }
}
