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

import java.io.IOException;
import java.util.Properties;

import org.assertj.core.util.Strings;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;

@Disabled("Run it locally with correct test/resources/triplestore.properties settings")
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
    public void testStorePagingWorks()
            throws IOException, StoreAccessException, ModelUnmarshallingException {
        super.testStorePagingWorks();
    }

    @BeforeEach
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
