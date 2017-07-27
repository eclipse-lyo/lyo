package org.eclipse.lyo.store;

/*-
 * #%L
 * Contributors:
 *     Andrew Berezovskyi - initial implementation
 *     Jad El-khoury      - extensive review and improvement of the API, javadoc improvement
 * %%
 * Copyright (C) 2016 KTH Royal Institute of Technology and others.
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

import com.hp.hpl.jena.rdf.model.Model;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import org.eclipse.lyo.oslc4j.core.model.IResource;

/**
 * Store is the main interface for operations on OSLC Resources with the backing
 * triplestore. Please use {@link StoreFactory} to instantiate concrete implementations.
 * <p>
 * <p>The store is designed to be used for storing individual {@link IResource}s or
 * unordered sets of them (due to how triplestores work).</p>
 * <p>
 * <p>Resources are stored under a specific named graph similar to how the keys are used in the
 * {@link java.util.Map}. That is, a <em>unique named graph</em> must be supplied to store a
 * (group of)
 * resource(s) under the same <em>named graph</em> in the triplestore. The named graph is
 * expected to be
 * a valid {@link URI}.
 * </p>
 * <p>
 * <p>In the case of an individual resource, you can use the resource's <em>about URI</em> as the
 * <code>namedGraph</code>.</p>
 * <p>
 * Example uses:
 * <ul>
 * <li><code>store.containsKey(resource.getAbout().toString())</code> - returns whether a
 * resource is stored under its named graph URI</li>
 * <li><code>store.putResources(resource.getAbout().toString(), resource)</code> - store the
 * resource under the namedGraph that corresponds to the <em>about URI</em> of the resource.
 * Equivalent
 * of <code>store.putResource(resource)</code></li>
 * <li><code>store.putResources(serviceProviderId, resources)</code> - store all resources
 * managed by a given Service Provider, under a named graph identified by the
 * <code>serviceProviderId</code> URI. This assumes that the
 * <code>serviceProviderId</code> is a
 * valid URI.</li>
 * </ul>
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public interface Store {

    /**
     * Insert a jena model into the named graph with corresponding URI.
     */
    void insertJenaModel(URI namedGraphUri, Model model);

    /**
     * Insert Jena models representations of OSLC {@link IResource} instances into the
     * named graph.
     * existing statements already present in the named graph are not overridden, nor removed.
     */
    boolean insertResources(URI namedGraphUri, final Object... resources)
            throws StoreAccessException;

    /**
     * Delete all statements whose subject is one of nodeUris
     */
    void deleteResources(URI namedGraphUri, final URI... nodeUris);

    /**
     * Delete all statements whose subject is one of the resources's URis.
     */
    void deleteResources(URI namedGraphUri, final IResource... resources);

    /**
     * Checks if the triplestore has the named graph under the corresponding URI.
     *
     * @param namedGraphUri URI of a named graph that shall be a valid URI and was used before with
     *                      the <code>put*</code> methods.
     *
     * @return does the triplestore contain any resources for a given named graph
     */
    boolean namedGraphExists(URI namedGraphUri);

    /**
     * Checks if a resource with resourceUri exists under the corresponding named graph.
     */
    boolean resourceExists(URI namedGraphUri, URI resourceUri);

    /**
     * Retrieve a Jena model for triples under the given subject from the corresponding named graph.
     */
    Model getJenaModelForSubject(URI namedGraphUri, URI subject)
            throws NoSuchElementException, StoreAccessException, ModelUnmarshallingException;

    /**
     * Retrieve the collection of {@link IResource} instances specified by the concrete
     * type, unmarshaled from the RDF graph persisted in the triplestore under the given
     * named graph. The
     * named graph must represent a URI that was previously given to another method call or a
     * Resource
     * about URI if the resource was saved alone.
     *
     * @param clazz Concrete type of the stored resources. Must be correct (correspond to the
     *              marshaled type and be a subclass of {@link IResource}), otherwise the
     *              instantiation from model will not succeed.
     *
     * @return A collection of {@link IResource} instances, unmarshaled from the RDF graph persisted
     * in the triplestore.
     *
     * @throws StoreAccessException        if there was a problem with the triplestore (or the
     *                                     dataset, more broadly).
     * @throws ModelUnmarshallingException if the classes cannot be instantiated or another error
     *                                     occurred when working with Jena model.
     */
    <T extends IResource> List<T> getResources(URI namedGraphUri, final Class<T> clazz)
            throws StoreAccessException, ModelUnmarshallingException;

    /**
     * Alternative to {@link Store#getResources(URI, Class)} with paging on the OSLC resource level.
     *
     * @param namedGraphUri URI of a named graph under which resources were stored
     * @param clazz         class of the resources being retrieved
     * @param limit         paging limit
     * @param offset        paging offset
     *
     * @return list of OSLC resources, size is less or equal to 'limit'
     *
     * @throws StoreAccessException        if there was a problem with the triplestore (or the
     *                                     dataset, more broadly).
     * @throws ModelUnmarshallingException if the classes cannot be instantiated or another error
     *                                     occurred when working with Jena model.
     */
    <T extends IResource> List<T> getResources(URI namedGraphUri, Class<T> clazz, int limit,
            int offset) throws StoreAccessException, ModelUnmarshallingException;

    /**
     * Retrieve a single {@link IResource} instance specified by the concrete
     * type, unmarshaled from the RDF graph persisted in the triplestore under the given
     * named graph.
     * namedGraph must represent a URI that was previously given to another call.
     *
     * @param namedGraphUri URI of a named graph under which resources were stored
     * @param clazz         Java class of the stored resources.
     *
     * @return An {@link IResource} instance, unmarshaled from the RDF graph persisted in the
     * triplestore.
     *
     * @throws NoSuchElementException      if the resource is not present under a given named graph
     *                                     URI or if the named graph itself contains no resources
     *                                     (effectively missing).
     * @throws StoreAccessException        if there was a problem with the triplestore (or the
     *                                     dataset, more broadly).
     * @throws ModelUnmarshallingException if the classes cannot be instantiated or another error
     *                                     occurred when working with Jena model.
     */
    <T extends IResource> T getResource(URI namedGraphUri, URI uri, Class<T> clazz)
            throws NoSuchElementException, StoreAccessException, ModelUnmarshallingException;

    /**
     * Insert Jena models representations of OSLC {@link IResource} instances into the
     * named graph.
     * Any existing model representations of these instances are overridden.
     * other existing statements in the named graph are not overridden, nor removed.
     */
    <T extends IResource> boolean updateResources(URI namedGraphUri, final T... resources)
            throws StoreAccessException;

    /**
     * Inserts OSLC {@link IResource} instances into the triplestore in the designed
     * namedGraph.
     * <p>Any previously stored resource(s) and/or statements will be overwritten.</p>
     *
     * @param namedGraphUri A unique URI that is used as a named graph to which resources are
     *                      appended
     * @param resources     A collection of {@link IResource} instances
     *
     * @return Operation success
     *
     * @throws StoreAccessException if the operation can't be performed
     */
    <T extends IResource> boolean putResources(URI namedGraphUri, final Collection<T> resources)
            throws StoreAccessException;

    /**
     * Adds OSLC {@link IResource} instances in addition to other instances already
     * present on this named graph into the triplestore. Jena models representing existing and added
     * resource arrays will be combined.
     *
     * @param namedGraphUri A unique URI that is used as a named graph to which resources are
     *                      appended
     * @param resources     A collection of {@link IResource} instances
     *
     * @return Operation success
     *
     * @throws StoreAccessException if the operation can't be performed
     */
    <T extends IResource> boolean appendResources(URI namedGraphUri, final Collection<T> resources)
            throws StoreAccessException;

    /**
     * Adds an OSLC {@link IResource} instance in addition to other instances already
     * present on this named graph into the triplestore. Jena models representing existing and added
     * resource arrays will be combined.
     *
     * @param namedGraphUri A unique URI that is used as a named graph to which resources are
     *                      appended
     * @param resource      An {@link IResource} instance with 'about' property set
     *
     * @return Operation success
     *
     * @throws StoreAccessException if the operation can't be performed
     */
    default <T extends IResource> boolean appendResource(URI namedGraphUri, final T resource)
            throws StoreAccessException {
        return appendResources(namedGraphUri, Collections.singletonList(resource));
    }

    /**
     * Deletes the resources and statements stored under the given named graph from the triplestore.
     * <p>
     * <p>May delete the containing named graph, many triplestores will delete the named graph
     * automatically as well as soon as it doesn't contain any triples.
     * </p>
     */
    void clear(URI namedGraphUri);

    /**
     * Enumerates all named graph URIs within the dataset.
     * <p>
     * <p>If the dataset was not manipulated by other means than via this interface, it can be
     * assumed that all URIs in the set are valid keys.
     * </p>
     *
     * @return The set of all named graph URIs, may contain URIs that are not valid keys if the
     * dataset was manipulated directly.
     */
    Set<String> keySet();

    /**
     * Remove EVERYTHING from the triplestore.
     *
     * @since 0.23.0
     */
    void removeAll();
}
