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

package org.eclipse.lyo.store;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;

import org.apache.jena.query.Dataset;
import org.eclipse.lyo.store.internals.DatasetBuilder;
import org.junit.Ignore;
import org.junit.Test;

public class DatasetBuilderTest {

    private static final String PATH_PREFIX = "jenaTest";
    private Dataset dataset;

    @Test
    public void testNewDatasetIsEmpty() throws IOException {
        final Path tempDirectory = Files.createTempDirectory(DatasetBuilderTest.PATH_PREFIX);
        dataset = DatasetBuilder.buildPersistent(tempDirectory);
        final Iterator<String> names = dataset.listNames();

        assertThat(names).isExhausted();
    }


    @Ignore("Because of Gitlab CI container permissions")
    @Test(expected = IOException.class)
    public void testWrongPathCausesExceptionNoPermissions() throws IOException {
        final Path tempDirectory = Paths.get("/root/test");
        dataset = DatasetBuilder.buildPersistent(tempDirectory);
        final Iterator<String> names = dataset.listNames();

        assertThat(names).isExhausted();
    }
}
