package org.eclipse.lyo.store;

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

import org.apache.jena.query.Dataset;
import java.io.File;
import java.io.IOException;
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
}
