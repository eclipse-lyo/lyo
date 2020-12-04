package org.eclipse.lyo.store;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

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
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDBFactory;
import org.assertj.core.api.Assertions;
import org.eclipse.lyo.store.internals.JenaTdbStoreImpl;
import org.eclipse.lyo.store.resources.Requirement;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

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
