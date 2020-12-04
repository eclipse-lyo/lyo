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

import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.eclipse.lyo.store.internals.query.DatasetQueryExecutorImpl;
import org.junit.Before;
import org.junit.Ignore;

/**
 * DatasetBuilderTest is .
 * @author Andrew Berezovskyi <andriib@kth.se>
 * @since 2016-11-01
 */
@SuppressWarnings("PMD.LongVariable")
public class SparqlStoreImplTest extends StoreTestBase<SparqlStoreImpl> {

    private SparqlStoreImpl manager;

    @Before
    public void setUp() throws Exception {
        manager = new SparqlStoreImpl(new DatasetQueryExecutorImpl());
    }

    @Override
    protected SparqlStoreImpl buildStore() {
        return manager;
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreKeySetReturnsCorrectKeys() {}
}
