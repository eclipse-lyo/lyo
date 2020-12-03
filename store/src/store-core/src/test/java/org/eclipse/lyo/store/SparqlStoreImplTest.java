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
