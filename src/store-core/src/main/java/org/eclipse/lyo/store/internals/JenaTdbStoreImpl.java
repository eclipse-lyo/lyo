package org.eclipse.lyo.store.internals;

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

import com.google.common.collect.Sets;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import javax.xml.datatype.DatatypeConfigurationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.store.ModelUnmarshallingException;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.StoreAccessException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link Store} implementation based on a Jena triplestore that can be either backed by
 * in-memory or on-disk dataset.
 *
 * @version $version-stub$
 * @since 0.11.0
 */
public class JenaTdbStoreImpl implements Store {
    private static final Logger log = LoggerFactory.getLogger(JenaTdbStoreImpl.class);
    private final Dataset dataset;

    /**
     * Initialise Store backed by an in-memory dataset. All the data will be erased once the
     * dataset is destructed.
     */
    public JenaTdbStoreImpl() {
        this.dataset = TDBFactory.createDataset();
    }

    /**
     * Initialise Store backed by a provided dataset.
     *
     * @param dataset an instance of the Dataset for the store
     */
    public JenaTdbStoreImpl(final Dataset dataset) {
        this.dataset = dataset;
    }

    @Override
    public boolean namedGraphExists(final URI namedGraph) {
        boolean contains;
        dataset.begin(ReadWrite.READ);
        try {
            contains = dataset.containsNamedModel(namedGraph.toString());
        } finally {
            dataset.end();
        }
        return contains;
    }

    @Override
    public <T extends IResource> List<T> getResources(final URI namedGraph,
            final Class<T> clazz) throws StoreAccessException, ModelUnmarshallingException {
        if (namedGraphExists(namedGraph)) {
            dataset.begin(ReadWrite.READ);
            final Model model = dataset.getNamedModel(namedGraph.toString());
            dataset.end();
            try {
                final Object[] obj = JenaModelHelper.fromJenaModel(model, clazz);
                @SuppressWarnings("unchecked") final T[] castObjects = (T[]) Array.newInstance(
                        clazz, obj.length);
                for (int i = 0; i < obj.length; i++) {
                    castObjects[i] = clazz.cast(obj[i]);
                }
                return Arrays.asList(castObjects);
            } catch (DatatypeConfigurationException | IllegalAccessException |
                    InstantiationException | OslcCoreApplicationException |
                    InvocationTargetException | NoSuchMethodException | URISyntaxException e) {
                throw new ModelUnmarshallingException(
                        "Failed to buildPersistent an " + "object from Jena model", e);
            }
        } else {
            throw new StoreAccessException(
                    String.format("Model with namedGraph='%s'" + " is not cached", namedGraph));
        }
    }

    @Override
    public final <T extends IResource> boolean putResources(final URI namedGraph,
            final Collection<T> resources) throws StoreAccessException {
        try {
            final Model model = JenaModelHelper.createJenaModel(resources.toArray());
            insertJenaModel(namedGraph, model);
            return true;
        } catch (DatatypeConfigurationException | IllegalAccessException |
                OslcCoreApplicationException | InvocationTargetException e) {
            throw new StoreAccessException(e);
        }
    }

    @Override
    public final <T extends IResource> boolean appendResources(final URI namedGraph,
            final Collection<T> resources) throws StoreAccessException {
        dataset.begin(ReadWrite.WRITE);
        try {
            final Model triplestoreModel = dataset.getNamedModel(String.valueOf(namedGraph));
            final Model addModel = JenaModelHelper.createJenaModel(resources.toArray());
            triplestoreModel.add(addModel);
            dataset.commit();
        } catch (IllegalAccessException | DatatypeConfigurationException |
                OslcCoreApplicationException | InvocationTargetException e) {
            throw new StoreAccessException(e);
        } finally {
            dataset.end();
        }
        return false;
    }

    @Override
    public void clear(final URI namedGraph) {
        dataset.begin(ReadWrite.WRITE);
        try {
            dataset.removeNamedModel(namedGraph.toString());
            dataset.commit();
        } finally {
            dataset.end();
        }
    }

    @Override
    public <T extends IResource> List<T> getResources(final URI namedGraph,
            final Class<T> clazz, final int limit, final int offset)
            throws StoreAccessException, ModelUnmarshallingException {
        // TODO: 15.02.17 add proper impl
        List<T> resources = getResources(namedGraph, clazz);
        return resources.subList(offset, Math.min(resources.size(), offset + limit));
    }

    @Override
    public Set<String> keySet() {
        dataset.begin(ReadWrite.READ);
        try {
            final Iterator<String> namedGraphNames = dataset.listNames();
            return Sets.newHashSet(namedGraphNames);
        } finally {
            dataset.end();
        }
    }

    @Override
    public void removeAll() {
        // TODO Andrew@2017-04-04: do we need a transaction here?
        dataset.asDatasetGraph().clear();
    }

    @Override
    public <T extends IResource> T getResource(final URI namedGraph, final URI uri,
            final Class<T> clazz)
            throws NoSuchElementException, StoreAccessException, ModelUnmarshallingException {
        final List<T> list = getResources(namedGraph, clazz);
        for (final T resource : list) {
            if (resource.getAbout().equals(uri)) {
                return resource;
            }
        }
        throw new NoSuchElementException(
                "Resource with a given URI doesn't exist under a given namedGraph");
    }

    @Override
    public void insertJenaModel(final URI name, final Model model) {
        dataset.begin(ReadWrite.WRITE);
        try {
            dataset.addNamedModel(name.toString(), model);
            dataset.commit();
        } finally {
            dataset.end();
        }
        TDB.sync(dataset);
    }

    @Override
    public boolean resourceExists(URI namedGraphUri, URI resourceUri) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean insertResources(URI namedGraph, Object... resources)
            throws StoreAccessException {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deleteResources(URI namedGraphUri, URI... nodeUris) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deleteResources(URI namedGraphUri, IResource... resources) {
        throw new UnsupportedOperationException();
    }

    @Override
    public <T extends IResource> boolean updateResources(URI namedGraphUri, T... resources)
            throws StoreAccessException {
        throw new UnsupportedOperationException();
    }

    @Override
    public Model getJenaModelForSubject(URI namedGraphUri, URI subject)
            throws NoSuchElementException, StoreAccessException, ModelUnmarshallingException {
        throw new UnsupportedOperationException();
    }

    public boolean putResource(URI namedGraphUri, IResource resource)
            throws StoreAccessException, NoSuchElementException {
        throw new UnsupportedOperationException();
    }

}
