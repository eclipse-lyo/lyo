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
import org.eclipse.lyo.server.jaxrs.services.ResourceId;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.internals.SparqlStoreImpl;
import org.eclipse.lyo.store.internals.query.DatasetQueryExecutorImpl;
import org.junit.Test;


public class StoreRepoTest {
    @Test
    public void classCapturedCorrectlyK() throws RepositoryConnectionException, RepositoryOperationException {
        // in-mem SPARQL-based Store
        Store store = new SparqlStoreImpl(new DatasetQueryExecutorImpl());

        ResourceRepository<ServiceProvider, ServiceProviderId> repository = new LyoStoreKRepositoryImpl<>(store,
            ServiceProvider.class, URI.create("urn:lyo:default"));

        Optional<ServiceProvider> resource = repository.getResource(new ServiceProviderId(URI.create("urn:lyo:nonexistent")));

        assertFalse(resource.isPresent());
        assertEquals(((LyoStoreKRepositoryImpl<ServiceProvider, ServiceProviderId>) repository).getResourceClass(),
            ServiceProvider.class);
    }

    static class ServiceProviderId implements ResourceId<ServiceProvider> {
        private URI uri;

        public ServiceProviderId() {
        }

        public ServiceProviderId(URI uri) {
            this.uri = uri;
        }

        @Override
        public URI toUri() {
            return uri;
        }
    }

}
