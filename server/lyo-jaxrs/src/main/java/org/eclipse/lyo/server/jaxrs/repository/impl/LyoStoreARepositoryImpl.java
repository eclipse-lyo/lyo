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

import java.lang.reflect.Type;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.UUID;

import javax.inject.Inject;
import javax.inject.Named;
import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryConnectionException;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;
import org.eclipse.lyo.server.jaxrs.repository.ResourceRepository;
import org.eclipse.lyo.server.jaxrs.services.Delegate;
import org.eclipse.lyo.server.jaxrs.services.ResourceId;
import org.eclipse.lyo.store.ModelUnmarshallingException;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.StoreAccessException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.jsonldjava.shaded.com.google.common.collect.ImmutableList;
import com.google.common.reflect.TypeToken;

// TODO add a javadoc and describe the difference between the two Store-backed implementations
// Requires a subclass to be created on the fly, see org.eclipse.lyo.server.jaxrs.repository.impl.StoreRepoTest.classCapturedCorrectlyAn
@Deprecated
public class LyoStoreARepositoryImpl<RT extends AbstractResource, IDT extends ResourceId<RT>> implements ResourceRepository<RT, IDT> {
    private final static Logger LOG = LoggerFactory.getLogger(LyoStoreARepositoryImpl.class);
    private final Store store;
    private final Class<RT> resourceClass;
    private final TypeToken<RT> resourceType = new TypeToken<RT>(getClass()){};
    private final URI namedGraph;

    @Inject
    public LyoStoreARepositoryImpl(Store store, @Named("store-named-graph") URI namedGraph) {
        this.store = store;
        this.namedGraph = namedGraph;
        Type rawType = resourceType.getRawType();
        if (rawType instanceof Class) {
            resourceClass = (Class<RT>) rawType;
        } else {
            throw new IllegalStateException("The repository must be parametrized with a class");
        }
    }

    @Override
    public Optional<RT> getResource(IDT id) throws RepositoryOperationException {
        URI uri = id.toUri();
        try {
            RT resource = store.getResource(namedGraph, uri, resourceClass);
            return Optional.of(resource);
        } catch (StoreAccessException e) {
            LOG.warn("Connection to the Store endpoint failed");
            throw new RepositoryConnectionException(e);
        } catch (ModelUnmarshallingException e) {
            throw new RepositoryOperationException("Store Model cannot be unmarshalled");
        } catch (NoSuchElementException ignored) {
            return Optional.empty();
        }
    }


    // package-private methods for testing
    Class<RT> getResourceClass() {
        return resourceClass;
    }

    @Override
    public boolean deleteResource(IDT id) {
        try {
            store.deleteResources(namedGraph, id.toUri());
            return true;
        } catch (Exception e) {
            LOG.debug("Unknown exception happened while deleting {} from Store:", id, e);
            return false;
        }
    }

    @Override
    public RT update(RT updatedResource, IDT id, Class<RT> klass) throws RepositoryOperationException {
        URI uri = id.toUri();
        if(uri == null) {
            throw new IllegalArgumentException("Id object may not produce a null URI");
        }
        if(!uri.equals(updatedResource.getAbout())) {
            throw new IllegalArgumentException("URI of the resource and the Id object do not match");
        }
        try {
            updateETag(updatedResource, false);
            boolean success = store.updateResources(namedGraph, updatedResource);
            if (success) {
                return updatedResource;
            } else {
                throw new RepositoryOperationException("Failed to update a Resource in an RDF Store");
            }
        } catch (StoreAccessException e) {
            LOG.warn("Connection to the Store endpoint failed");
            throw new RepositoryConnectionException();
        }
    }

    @Override
    public List<RT> queryResources(String oslcWhere, String oslcPrefixes, int page,
                                   int pageSize) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getETag(RT resource) {
        Map<QName, Object> props = resource.getExtendedProperties();
        if(!props.containsKey(Delegate.OSLC_ETAG)) {
            try {
                updateETag(resource);
            } catch (StoreAccessException e) {
                LOG.error("Failed to generate an initial ETag ({})", resource.getAbout());
            }
        }
        return props.get(Delegate.OSLC_ETAG).toString();
    }

    private void updateETag(RT resource) throws StoreAccessException {
        updateETag(resource, true);
    }

    private void updateETag(RT resource, boolean update) throws StoreAccessException {
        String newETag = generateETag();
        resource.getExtendedProperties().put(Delegate.OSLC_ETAG, newETag);
        if(update) {
            store.updateResources(namedGraph, resource);
        }
    }

    private String generateETag() {
        return UUID.randomUUID().toString();
    }

    @Override
    public RT createResource(RT aResource, IDT id, Class<RT> klass)
            throws RepositoryOperationException {
        URI uri = id.toUri();
        try {
            if(aResource.getAbout() != null && aResource.getAbout() != uri) {
                LOG.debug("Overriding resource URI {} with {}", aResource.getAbout(), uri);
            }
            aResource.setAbout(uri);

            updateETag(aResource, false);
            boolean success = store.putResources(namedGraph, ImmutableList.of(aResource));
            if (success) {
                return aResource;
            } else {
                throw new RepositoryOperationException("Failed to put a Resource in an RDF Store");
            }
        } catch (StoreAccessException e) {
            throw new RepositoryConnectionException(e);
        }
    }
}
