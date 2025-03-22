package org.eclipse.lyo.store;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

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

public class SparqlStoreImplTest extends StoreTestBase<SparqlStoreImpl> {

    private Store manager;

    @BeforeEach
    public void setUp() {
        manager = StoreFactory.sparqlInMem();
    }

    @Override
    protected Store buildStore() {
        return manager;
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
        var start = Instant.now();
        for (int i = 0; i < 10; i++) {
            final URI testNg = URI.create("urn:test:" + i);
            try {
                manager.putResources(testNg, providers);
            } catch (StoreAccessException e) {
                fail("Store failed", e);
            }
        }
        System.out.printf("10 named graphs persisted (resources) in %s ms", Duration.between(start, Instant.now()).toMillis());
    }

    @Test
    public void testInsertionPerfRaw() throws InvocationTargetException, DatatypeConfigurationException,
            OslcCoreApplicationException, IllegalAccessException {
        final List<ServiceProvider> providers = genProviders();
        final Model jenaModel = JenaModelHelper.createJenaModel(providers.toArray());
        var start = Instant.now();
        for (int i = 0; i < 10; i++) {
            final URI testNg = URI.create("urn:test:" + i);
            manager.insertJenaModel(testNg, jenaModel);
        }
        System.out.printf("10 named graphs persisted (raw Model) in %s ms", Duration.between(start, Instant.now()).toMillis());
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
