/*
 * Copyright (c) 2021 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.server.jaxrs.repository.impl;

import static org.junit.Assert.*;

import java.net.URI;
import java.util.Optional;

import org.eclipse.lyo.oslc4j.core.model.ServiceProvider;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryConnectionException;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;
import org.eclipse.lyo.server.jaxrs.repository.ResourceRepository;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.eclipse.lyo.store.internals.query.DatasetQueryExecutorImpl;
import org.junit.Test;


public class StoreRepoTest {
    @Test
    public void classCapturedCorrectlyAn() throws RepositoryConnectionException, RepositoryOperationException {
        // in-mem SPARQL-based Store
        Store store = new SparqlStoreImpl(new DatasetQueryExecutorImpl());

        // this WON'T WORK!
        // StoreRepositoryImpl<ServiceProvider> repository = new StoreRepositoryImpl<ServiceProvider>(store, URI.create("urn:lyo:default"));
        ResourceRepository<ServiceProvider> repository = new LyoStoreARepositoryImpl<ServiceProvider>(store, URI.create("urn:lyo:default")) {};

        Optional<ServiceProvider> resource = repository.getResource(URI.create("urn:lyo:nonexistent"));

        assertFalse(resource.isPresent());
        assertEquals(((LyoStoreARepositoryImpl<ServiceProvider>)repository).getResourceClass(), ServiceProvider.class);
    }

    @Test
    public void classCapturedCorrectlyK() throws RepositoryConnectionException, RepositoryOperationException {
        // in-mem SPARQL-based Store
        Store store = new SparqlStoreImpl(new DatasetQueryExecutorImpl());

        // this WON'T WORK!
        // StoreRepositoryImpl<ServiceProvider> repository = new StoreRepositoryImpl<ServiceProvider>(store, URI.create("urn:lyo:default"));
        ResourceRepository<ServiceProvider> repository = new LyoStoreKRepositoryImpl<ServiceProvider>(store, ServiceProvider.class, URI.create("urn:lyo:default"));

        Optional<ServiceProvider> resource = repository.getResource(URI.create("urn:lyo:nonexistent"));

        assertFalse(resource.isPresent());
        assertEquals(((LyoStoreKRepositoryImpl<ServiceProvider>)repository).getResourceClass(), ServiceProvider.class);
    }

}
