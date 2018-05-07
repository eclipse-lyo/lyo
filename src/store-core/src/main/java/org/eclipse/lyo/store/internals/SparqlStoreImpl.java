package org.eclipse.lyo.store.internals;

/*-
 * #%L
 * Contributors:
 *     Andrew Berezovskyi - initial implementation
 *     Jad El-khoury - methods that overwrite only updated resources
 * %%
 * Copyright (C) 2016 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.ParameterizedSparqlString;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolutionMap;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.rdf.model.impl.ResourceImpl;
import org.apache.jena.sparql.modify.request.QuadDataAcc;
import org.apache.jena.sparql.modify.request.UpdateDataInsert;
import org.apache.jena.update.UpdateProcessor;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import javax.xml.datatype.DatatypeConfigurationException;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.IResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.store.ModelUnmarshallingException;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.StoreAccessException;
import org.eclipse.lyo.store.internals.query.JenaQueryExecutor;
import org.eclipse.lyo.store.internals.query.SparqlQueryExecutorBasicAuthImpl;
import org.eclipse.lyo.store.internals.query.SparqlQueryExecutorImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link Store} interface implementation that interacts with any SPARQL-based triplestore
 * through a Query and Update endpoints.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public class SparqlStoreImpl implements Store {
    /**
     * Could be used to prevent extremely large results
     */
    public final static int TRIPLE_LIMIT = 10001;
    private final static Logger log = LoggerFactory.getLogger(JenaTdbStoreImpl.class);
    private final JenaQueryExecutor queryExecutor;

    /**
     * Initialises the Store with the endpoints for query and update. Must be available over HTTP
     * or HTTPS (TLS
     * &amp; cipher support depends on the JDK version) without authentication.
     *
     * @param queryEndpoint  SPARQL QUERY endpoint
     * @param updateEndpoint SPARQL UPDATE endpoint
     */
    public SparqlStoreImpl(final String queryEndpoint, final String updateEndpoint) {
        this(new SparqlQueryExecutorImpl(queryEndpoint, updateEndpoint));
    }

    /**
     * Initialises the Store with the endpoints for query and update. Must be available over HTTP
     * or HTTPS (TLS
     * &amp; cipher support depends on the JDK version) with authentication via username and
     * password
     * combinations. Authentication works with the basic and digest HTTP authentication schemes.
     *
     * @param queryEndpoint  SPARQL QUERY endpoint
     * @param updateEndpoint SPARQL UPDATE endpoint
     * @param username       Username
     * @param password       Password
     */
    public SparqlStoreImpl(final String queryEndpoint, final String updateEndpoint,
            final String username, final String password) {
        this(new SparqlQueryExecutorBasicAuthImpl(queryEndpoint, updateEndpoint, username,
                password));
    }

    /**
     * Initialises the Store with the custom {@link JenaQueryExecutor}.
     *
     * @param queryExecutor Instance of the {@link JenaQueryExecutor} that can run queries and updates.
     */
    public SparqlStoreImpl(final JenaQueryExecutor queryExecutor) {
        this.queryExecutor = queryExecutor;
    }

    @Override
    public void insertJenaModel(final URI namedGraph, final Model model) {
        final QuadDataAcc quadAccumulator = new QuadDataAcc();
        quadAccumulator.setGraph(NodeFactory.createURI(String.valueOf(namedGraph)));
        final StmtIterator statementIterator = model.listStatements();
        while (statementIterator.hasNext()) {
            final Statement statement = statementIterator.nextStatement();
            final Triple triple = statement.asTriple();
            quadAccumulator.addTriple(triple);
        }
        final UpdateDataInsert dataInsertUpdate = new UpdateDataInsert(quadAccumulator);
        final String queryString = dataInsertUpdate.toString();
        final UpdateProcessor up = queryExecutor.prepareSparqlUpdate(queryString);
        up.execute();
    }

    @Override
    public boolean insertResources(final URI namedGraph, final Object... resources)
            throws StoreAccessException {
        try {
            final Model model = JenaModelHelper.createJenaModel(resources);
            insertJenaModel(namedGraph, model);
            return true;
        } catch (DatatypeConfigurationException | IllegalAccessException |
                OslcCoreApplicationException | InvocationTargetException e) {
            throw new StoreAccessException(e);
        }
    }

    //this is highly inefficient. I need to replace with a single query that removes all nodes.
    @Override
    public void deleteResources(final URI namedGraphUri, final URI... subjectUris) {
        final QuerySolutionMap map = new QuerySolutionMap();
        for (int i = 0; i < subjectUris.length; i++) {
            map.clear();
            map.add("graph", new ResourceImpl(String.valueOf(namedGraphUri)));
            map.add("subject", new ResourceImpl(String.valueOf(subjectUris[i])));
            final ParameterizedSparqlString sparqlString = new ParameterizedSparqlString(
                    "WITH ?graph DELETE  { ?s ?p ?v } WHERE {?s ?p ?v . FILTER(?s = ?subject)}",
                    map);
            final String query = sparqlString.toString();
            final UpdateProcessor updateProcessor = queryExecutor.prepareSparqlUpdate(query);
            updateProcessor.execute();
        }
    }

    @Override
    public void deleteResources(final URI namedGraphUri, final IResource... resources) {
        URI[] nodeUris = new URI[resources.length];
        for (int i = 0; i < resources.length; i++) {
            nodeUris[i] = resources[i].getAbout();
        }
        deleteResources(namedGraphUri, nodeUris);
    }

    @Override
    public boolean namedGraphExists(final URI namedGraphUri) {
        final QuerySolutionMap map = new QuerySolutionMap();
        map.add("g", new ResourceImpl(String.valueOf(namedGraphUri)));
        final ParameterizedSparqlString sparqlString = new ParameterizedSparqlString(
                "ASK {GRAPH ?g {?s ?p ?o} }", map);
        final String query = sparqlString.toString();
        final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query);
        return queryExecution.execAsk();
    }

    @Override
    public boolean resourceExists(final URI namedGraphUri, final URI resourceUri) {
        final QuerySolutionMap map = new QuerySolutionMap();
        map.add("g", new ResourceImpl(String.valueOf(namedGraphUri)));
        map.add("s", new ResourceImpl(String.valueOf(resourceUri)));
        final ParameterizedSparqlString sparqlString = new ParameterizedSparqlString(
                "ASK {GRAPH ?g {?s ?p ?o} }", map);
        final String query = sparqlString.toString();
        final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query);
        return queryExecution.execAsk();
    }

    @Override
    public Model getJenaModelForSubject(final URI namedGraphUri, final URI subject)
            throws NoSuchElementException {
        if (namedGraphExists(namedGraphUri)) {
            final Model model;
            model = modelFromQueryByUri(namedGraphUri, subject);
            return model;
        } else {
            throw new NoSuchElementException("Cache namedGraph was missing from the triplestore");
        }
    }

    @Override
    public <T extends IResource> List<T> getResources(final URI namedGraph,
            final Class<T> clazz) throws StoreAccessException, ModelUnmarshallingException {
        if (namedGraphExists(namedGraph)) {
            final Model model;
            model = modelFromQueryFlat(namedGraph);
            return getResourcesFromModel(model, clazz);
        } else {
            throw new IllegalArgumentException("Named graph"
                                               + String.valueOf(namedGraph)
                                               + " was missing from "
                                               + "the triplestore");
        }
    }

    @Override
    public <T extends IResource> List<T> getResources(final URI namedGraph,
            final Class<T> clazz, final int limit, final int offset)
            throws StoreAccessException, ModelUnmarshallingException {
        if (namedGraphExists(namedGraph)) {
            final Model model;
            model = modelFromQueryFlatPaged(namedGraph, getResourceNsUri(clazz), limit, offset);
            return getResourcesFromModel(model, clazz);
        } else {
            throw new IllegalArgumentException("Named graph"
                                               + String.valueOf(namedGraph)
                                               + " was missing from "
                                               + "the triplestore");
        }
    }

    @Override
    public <T extends IResource> T getResource(final URI namedGraphUri,
            final URI resourceUri, final Class<T> clazz)
            throws NoSuchElementException, StoreAccessException, ModelUnmarshallingException {
        final Model model = getJenaModelForSubject(namedGraphUri, resourceUri);
        final List<T> modelResources = getResourcesFromModel(model, clazz);
        if (modelResources == null || modelResources.isEmpty()) {
            throw new NoSuchElementException(
                    "Empty Jena model for the subject " + resourceUri + ". Use resourceExists(g," +
                            "r) method to check for resource existence before calling this method" +
                            ".");
        }
        return modelResources.get(0);
    }

    @Override
    public <T extends IResource> boolean updateResources(final URI namedGraphUri,
            final T... resources) throws StoreAccessException {
        //No need to check if the resource exists. just delete it - if it is there.
        deleteResources(namedGraphUri, resources);
        return insertResources(namedGraphUri, resources);
    }

    @Override
    public <T extends IResource> boolean putResources(final URI uri,
            final Collection<T> resources) throws StoreAccessException {
        if (namedGraphExists(uri)) {
            clear(uri);
        }
        return insertResources(uri, resources.toArray());
    }

    @Override
    public <T extends IResource> boolean appendResources(final URI namedGraph,
            final Collection<T> resources) throws StoreAccessException {
        return insertResources(namedGraph, resources.toArray());
    }

    @Override
    public void clear(final URI namedGraph) {
        final QuerySolutionMap map = getGraphMap(namedGraph);
        final ParameterizedSparqlString query = new ParameterizedSparqlString("CLEAR GRAPH ?g",
                map);
        final UpdateProcessor up = queryExecutor.prepareSparqlUpdate(query.toString());
        up.execute();
    }

    @Override
    public Set<String> keySet() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void removeAll() {
        queryExecutor.prepareSparqlUpdate("CLEAR ALL").execute();
    }

    private <T extends IResource> List<T> getResourcesFromModel(final Model model,
            final Class<T> clazz) throws ModelUnmarshallingException, StoreAccessException {
        try {
            final Object[] obj = JenaModelHelper.fromJenaModel(model, clazz);
            @SuppressWarnings("unchecked") final T[] castObjects = (T[]) Array.newInstance(clazz,
                    obj.length);
            for (int i = 0; i < obj.length; i++) {
                castObjects[i] = clazz.cast(obj[i]);
            }
            return Arrays.asList(castObjects);
        } catch (InvocationTargetException | OslcCoreApplicationException | NoSuchMethodException
                | URISyntaxException | DatatypeConfigurationException | InstantiationException e) {
            throw new ModelUnmarshallingException(e);
        } catch (final IllegalAccessException e) {
            throw new StoreAccessException(e);
        }
    }

    private <T extends IResource> URI getResourceNsUri(final Class<T> clazz) {
        final OslcNamespace oslcNamespace = clazz.getAnnotation(OslcNamespace.class);
        final OslcName oslcName = clazz.getAnnotation(OslcName.class);

        final String name;
        if (oslcName != null) {
            name = oslcName.value();
        } else {
            name = clazz.getSimpleName();
        }
        return URI.create(oslcNamespace.value() + name);
    }

    private QuerySolutionMap getGraphMap(final URI namedGraph) {
        final QuerySolutionMap map = new QuerySolutionMap();
        map.add("g", new ResourceImpl(String.valueOf(namedGraph)));
        return map;
    }

    private Model modelFromQueryFlat(final URI namedGraph) {
        // TODO avoid CONSTRUCT query
        final QuerySolutionMap map = getGraphMap(namedGraph);
        final String queryTemplate = "DESCRIBE ?s WHERE { GRAPH ?g { ?s "
                                     + "?p "
                                     + "?o } }";
        final ParameterizedSparqlString query = new ParameterizedSparqlString(queryTemplate, map);

        final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query.toString());
        return queryExecution.execDescribe();
    }

    private Model modelFromQueryByUri(final URI namedGraph, final URI uri) {
        // TODO avoid CONSTRUCT query
        final QuerySolutionMap map = getGraphMap(namedGraph);
        map.add("s", new ResourceImpl(String.valueOf(uri)));
        final String queryTemplate = "DESCRIBE ?s WHERE { GRAPH ?g { ?s ?p ?o . } }";
        final ParameterizedSparqlString query = new ParameterizedSparqlString(queryTemplate, map);

        final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query.toString());
        return queryExecution.execDescribe();
    }

    private Model modelFromQueryFlatPaged(final URI namedGraph, final URI type, final int limit,
            final int offset) {
        // TODO avoid CONSTRUCT query
        final Model m = ModelFactory.createDefaultModel();
        final Resource typeResource = m.createResource(type.toString());

        final QuerySolutionMap map = getGraphMap(namedGraph);
        map.add("t", typeResource);
        final String queryTemplate = "PREFIX rdf: <http://www"
                                     + ".w3.org/1999/02/22-rdf-syntax-ns#>\n"
                                     + "DESCRIBE ?s\n"
                                     + "WHERE {\n"
                                     + "  GRAPH ?g {\n"
                                     + "    ?s ?p ?o\n"
                                     + "    {\n"
                                     + "      SELECT DISTINCT ?s\n"
                                     + "      WHERE {\n"
                                     + "        ?s ?p ?o .\n"
                                     + "        ?s rdf:type ?t.\n"
                                     + "   }\n"
                                     + "      LIMIT ?l\n"
                                     + "      OFFSET "
                                     + "?f\n"
                                     + "}\n"
                                     + "}\n"
                                     + "}\n";

        // TODO: 15.02.17 add global triple limit just in case
        //                + "\n" + "LIMIT " + TRIPLE_LIMIT + "\n" + "\n" + "\n";

        final ParameterizedSparqlString query = new ParameterizedSparqlString(queryTemplate, map);
        query.setLiteral("l", limit);
        query.setLiteral("f", offset);

        final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query.toString());
        return queryExecution.execDescribe();
    }

}
