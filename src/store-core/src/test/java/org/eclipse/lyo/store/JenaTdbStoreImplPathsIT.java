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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eclipse.lyo.store.internals.DatasetBuilder;
import org.eclipse.lyo.store.internals.JenaTdbStoreImpl;
import org.junit.Before;

/**
 * DatasetBuilderTest is .
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @since 0.15.2
 */
@SuppressWarnings("PMD.LongVariable")
public class JenaTdbStoreImplPathsIT extends StoreTestBase<JenaTdbStoreImpl> {

    private Path tempDirectory;

    @Before
    public void setUp() throws Exception {
        // use in-mem
        tempDirectory = Files.createTempDirectory("jenaTdbPrefix");
    }

    @Override
    protected JenaTdbStoreImpl buildStore() {
        try {
            return new JenaTdbStoreImpl(DatasetBuilder.buildPersistent(tempDirectory));
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }

}
