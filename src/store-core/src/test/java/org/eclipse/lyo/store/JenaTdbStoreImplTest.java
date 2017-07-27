package org.eclipse.lyo.store;

/*-
 * #%L
 * Contributors:
 * - Andrew Berezovskyi
 * %%
 * Copyright (C) 2016 - 2017 KTH Royal Institute of Technology and others.
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

import com.hp.hpl.jena.query.Dataset;
import com.hp.hpl.jena.tdb.TDBFactory;
import org.eclipse.lyo.store.internals.JenaTdbStoreImpl;
import org.junit.Before;

/**
 * DatasetBuilderTest is .
 * @author Andrew Berezovskyi <andriib@kth.se>
 * @since 2016-11-01
 */
@SuppressWarnings("PMD.LongVariable")
public class JenaTdbStoreImplTest extends StoreTestBase<JenaTdbStoreImpl> {

    private Dataset memDataset;

    @Before
    public void setUp() throws Exception {
        // use in-mem
        memDataset = TDBFactory.createDataset();
    }

    @Override
    protected JenaTdbStoreImpl buildStore() {
        return new JenaTdbStoreImpl(memDataset);
    }

}
