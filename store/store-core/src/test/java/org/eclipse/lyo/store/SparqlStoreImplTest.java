package org.eclipse.lyo.store;

/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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

import com.google.common.base.Stopwatch;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.tdb2.TDB2Factory;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.eclipse.lyo.store.internals.query.DatasetQueryExecutorImpl;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import javax.xml.datatype.DatatypeConfigurationException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;
import static org.junit.Assert.assertTrue;

public class SparqlStoreImplTest extends StoreTestBase<SparqlStoreImpl> {

    private SparqlStoreImpl manager;
    private Dataset dataset;

    @Before
    public void setUp() throws Exception {
        dataset = TDBFactory.createDataset(); // use in-mem implementation instead

//        final Path tdbDir = Files.createTempDirectory("lyo_tdb_");
//        System.out.println(tdbDir);
//        dataset = TDBFactory.createDataset(tdbDir.toAbsolutePath().toString());

        //FIXME make sure DatasetQueryExecutorImpl runs everything in a transaction
//        dataset = TDB2Factory.connectDataset(tdbDir.toAbsolutePath().toString());

        manager = new SparqlStoreImpl(new DatasetQueryExecutorImpl(dataset));
    }

    @Override
    protected SparqlStoreImpl buildStore() {
        return manager;
    }

    @Override
    @Ignore("Not implemented yet")
    public void testStoreKeySetReturnsCorrectKeys() {
    }

    @Test
    public void datasetIsPersistentAndEmpty() {
        assertTrue(TDB2Factory.isTDB2(dataset) || TDBFactory.isTDB1(dataset));
        try {
            dataset.begin(TxnType.READ);
            assertTrue(dataset.isEmpty());
        } finally {
            dataset.end();
        }
    }


    @Test
    public void storeBasicOps() {
        final URI testNg = URI.create("urn:test:1");
        ServiceProvider sp = new ServiceProvider();
        sp.setIdentifier("123");
        sp.setCreated(new Date());
        try {
            manager.putResources(testNg, Collections.singletonList(sp));
            final List<ServiceProvider> providers = manager.getResources(testNg, ServiceProvider.class);
            assertThat(providers).hasSize(1);
        } catch (StoreAccessException | ModelUnmarshallingException e) {
            fail("Store failed", e);
        }
    }

    @Test
    public void testInsertionPerf() {
        final List<ServiceProvider> providers = genProviders();
        final Stopwatch stopwatch = Stopwatch.createStarted();
        for (int i = 0; i < 100; i++) {
            final URI testNg = URI.create("urn:test:" + i);
            try {
                manager.putResources(testNg, providers);
//                final List<ServiceProvider> providers = manager.getResources(testNg, ServiceProvider.class);
//                assertThat(providers).hasSize(1);
            } catch (StoreAccessException e) {
                fail("Store failed", e);
            }
        }
        System.out.printf("100 named graphs persisted in %s", stopwatch.stop());
    }

    @Test
    public void testInsertionPerfRaw() throws InvocationTargetException, DatatypeConfigurationException,
            OslcCoreApplicationException, IllegalAccessException {
        final List<ServiceProvider> providers = genProviders();
        final Model jenaModel = JenaModelHelper.createJenaModel(providers.toArray());
        final Stopwatch stopwatch = Stopwatch.createStarted();
        for (int i = 0; i < 100; i++) {
            final URI testNg = URI.create("urn:test:" + i);
            manager.insertJenaModel(testNg, jenaModel);
        }
        System.out.printf("100 named graphs persisted in %s", stopwatch.stop());
    }

    private List<ServiceProvider> genProviders() {
        final List<ServiceProvider> providers = new ArrayList<>();
        for (int i = 0; i < 200; i++) {
            ServiceProvider sp = new ServiceProvider();
            sp.setIdentifier(String.valueOf(i));
            sp.setCreated(new Date());
            sp.setDescription("Defaulting to no-operation (NOP) logger implementation");
            providers.add(sp);
        }
        return providers;
    }


}
