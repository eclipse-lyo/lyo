package org.eclipse.lyo.store;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

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

}
