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
import java.util.Properties;
import org.assertj.core.util.Strings;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.junit.Before;
import org.junit.Ignore;

/**
 * SparqlStoreImplIT is .
 * @author Andrew Berezovskyi <andriib@kth.se>
 * @since 2016-11-02
 */
@Ignore("Run it locally with correct test/resources/triplestore.properties settings")
public class SparqlStoreImplIT extends StoreTestBase<SparqlStoreImpl> {
    private static final String SPARQL = "sparql";
    private static final String SPARUL = "sparul";
    private Properties triplestore;

    @Override
    protected SparqlStoreImpl buildStore() {
        final SparqlStoreImpl jenaSparqlStore = new SparqlStoreImpl(triplestore.getProperty(
                SparqlStoreImplIT.SPARQL), triplestore.getProperty(SparqlStoreImplIT.SPARUL));
        jenaSparqlStore.removeAll();
        return jenaSparqlStore;
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreKeySetReturnsCorrectKeys() {
    }

    @Override
    public void testStorePagingWorks()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        super.testStorePagingWorks();
    }

    @Before
    public void setUp() throws IOException {
        triplestore = new Properties();
        triplestore.load(SparqlStoreImplIT.class.getClassLoader()
                .getResourceAsStream("triplestore.properties"));
        if (Strings.isNullOrEmpty(triplestore.getProperty(SparqlStoreImplIT.SPARQL))) {
            throw new IllegalStateException(
                    "triplestore.properties file needs to be filled before running "
                    + "integration tests");
        }
    }
}
