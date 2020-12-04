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

import org.apache.jena.query.Dataset;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eclipse.lyo.store.internals.DatasetBuilder;
import org.eclipse.lyo.store.internals.JenaTdbStoreImpl;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * DatasetBuilderTest is .
 * @author Andrew Berezovskyi <andriib@kth.se>
 * @since 2016-11-01
 */
@SuppressWarnings("PMD.LongVariable")
public class JenaTdbStoreImplIT extends StoreTestBase<JenaTdbStoreImpl> {

    private Dataset dataset;

    @Before
    public void setUp() throws Exception {
        // use real persistent dataset
        dataset = DatasetBuilder.buildPersistent(Files.createTempDirectory("jenaTdbPrefix"));
    }

    @Override
    protected JenaTdbStoreImpl buildStore() {
        return new JenaTdbStoreImpl(dataset);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testStoreRefusesExistingFilePath() throws IOException {
        final Path tempDirectory = File.createTempFile("prefix", "tdb").toPath();
        StoreFactory.onDisk(tempDirectory);
    }

    @Ignore("Gitlab container doesn't fail this test")
    @Test(expected = IOException.class)
    public void testStoreHandlesPrivilegedPath() throws IOException {
        final Path tempDirectory = Paths.get("/root");
        StoreFactory.onDisk(tempDirectory);
    }

    @Test
    public void testStoreHandlesNonExistentPath() throws IOException {
        final Path tempDirectory = Paths.get("/tmp/try_tdb"); // make sure `ls -l /tmp` shows correct permissions
        StoreFactory.onDisk(tempDirectory);
    }
    
    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryForAllRequirementResources()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryForRequirementResourcesWithFreeTextSearch()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryForRequirementResourcesWithWhereFilter()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
   }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryForRequirementResourcesWithFreeTextSearchAndWhereFilter()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryForRequirementResourcesWithNoMatch()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryForAllResources()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryForAllResourcesWithFreeTextSearch()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryWithWhereFilterOnStringsWithIntegerValue()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreQueryWithWhereFilterOnIntegers()
            throws StoreAccessException, ModelUnmarshallingException, URISyntaxException {
    }

}
