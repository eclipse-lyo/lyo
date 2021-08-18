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

import org.eclipse.lyo.client.OSLCConstants;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryConnectionException;
import org.eclipse.lyo.server.jaxrs.repository.RepositoryOperationException;
import org.eclipse.lyo.server.jaxrs.repository.ResourceRepository;
import org.eclipse.lyo.store.ModelUnmarshallingException;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.StoreAccessException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.reflect.TypeToken;

// TODO add a javadoc and describe the difference between the two Store-backed implementations
public class LyoStoreARepositoryImpl<R extends AbstractResource> implements ResourceRepository<R> {
    // TODO use another namespace or raise an issue in the OP
    public static final QName OSLC_ETAG = new QName(OSLCConstants.OSLC_V2, "etag");
    private final static Logger LOG = LoggerFactory.getLogger(LyoStoreARepositoryImpl.class);
    private final Store store;
    private final Class<R> resourceClass;
    private final TypeToken<R> resourceType = new TypeToken<R>(getClass()){};
    private final URI namedGraph;

    @Inject
    public LyoStoreARepositoryImpl(Store store, @Named("store-named-graph") URI namedGraph) {
        this.store = store;
        this.namedGraph = namedGraph;
        Type rawType = resourceType.getRawType();
        if (rawType instanceof Class) {
            resourceClass = (Class<R>) rawType;
        } else {
            throw new IllegalStateException("The repository must be parametrized with a class");
        }
    }

    @Override
    public Optional<R> getResource(URI id) throws RepositoryConnectionException {
        try {
            R resource = store.getResource(namedGraph, id, resourceClass);
            return Optional.of(resource);
        } catch (StoreAccessException e) {
            LOG.warn("Connection to the Store endpoint failed");
            throw new RepositoryConnectionException();
        } catch (ModelUnmarshallingException e) {
            throw new IllegalStateException("Store Model cannot be unmarshalled");
        } catch (NoSuchElementException ignored) {
            return Optional.empty();
        }
    }


    // package-private methods for testing
    Class<R> getResourceClass() {
        return resourceClass;
    }

    @Override
    public boolean deleteResource(URI id) {
        try {
            store.deleteResources(namedGraph, id);
            return true;
        } catch (Exception e) {
            LOG.debug("Unknown exception happed while deleting {} from Store:", id, e);
            return false;
        }
    }

    @Override
    public R update(URI uri, R updatedResource, Class<R> klass) throws RepositoryConnectionException, RepositoryOperationException {
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
    public List<R> queryResources(String oslcWhere, String oslcPrefixes, int page,
            int pageSize) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getETag(R resource) {
        Map<QName, Object> props = resource.getExtendedProperties();
        if(!props.containsKey(OSLC_ETAG)) {
            try {
                updateETag(resource);
            } catch (StoreAccessException e) {
                LOG.error("Failed to generate an initial ETag ({})", resource.getAbout());
            }
        }
        return props.get(OSLC_ETAG).toString();
    }

    private void updateETag(R resource) throws StoreAccessException {
        updateETag(resource, true);
    }
    
    private void updateETag(R resource, boolean update) throws StoreAccessException {
        String newETag = generateETag();
        resource.getExtendedProperties().put(OSLC_ETAG, newETag);
        if(update) {
            store.updateResources(namedGraph, resource);
        }
    }

    private String generateETag() {
        String newUUID = UUID.randomUUID().toString();
        return newUUID;
    }
}
