package org.eclipse.lyo.store.internals;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.stream.Collectors;

import javax.xml.datatype.DatatypeConfigurationException;

/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
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

import org.apache.jena.arq.querybuilder.DescribeBuilder;
import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.Order;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.ParameterizedSparqlString;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolutionMap;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.rdf.model.StmtIterator;
import org.apache.jena.rdf.model.impl.ResourceImpl;
import org.apache.jena.riot.RiotException;
import org.apache.jena.sparql.expr.E_Regex;
import org.apache.jena.sparql.modify.request.QuadDataAcc;
import org.apache.jena.sparql.modify.request.UpdateDataInsert;
import org.apache.jena.update.UpdateProcessor;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.ComparisonTerm.Operator;
import org.eclipse.lyo.core.query.DecimalValue;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.SimpleTerm.Type;
import org.eclipse.lyo.core.query.StringValue;
import org.eclipse.lyo.core.query.UriRefValue;
import org.eclipse.lyo.core.query.Value;
import org.eclipse.lyo.core.query.WhereClause;
import org.eclipse.lyo.core.util.StringUtils;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
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
    public static final int TRIPLE_LIMIT = 10001;
    private static final Logger log = LoggerFactory.getLogger(SparqlStoreImpl.class);
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
        queryExecutor.beginWrite();
        try {
            final UpdateDataInsert dataInsertUpdate = new UpdateDataInsert(quadAccumulator);
            final UpdateProcessor up = queryExecutor.prepareSparqlUpdate(dataInsertUpdate);
            up.execute();
            queryExecutor.commit();
        } finally {
            queryExecutor.end();
        }
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
        for (URI uris : subjectUris) {
            map.clear();
            map.add("graph", new ResourceImpl(String.valueOf(namedGraphUri)));
            map.add("subject", new ResourceImpl(String.valueOf(uris)));
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

        queryExecutor.beginRead();
        try {
            final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query);
            return queryExecution.execAsk();
        } finally {
            queryExecutor.end();
        }
    }

    @Override
    public boolean resourceExists(final URI namedGraphUri, final URI resourceUri) {
        final QuerySolutionMap map = new QuerySolutionMap();
        map.add("g", new ResourceImpl(String.valueOf(namedGraphUri)));
        map.add("s", new ResourceImpl(String.valueOf(resourceUri)));
        final ParameterizedSparqlString sparqlString = new ParameterizedSparqlString(
                "ASK {GRAPH ?g {?s ?p ?o} }", map);
        final String query = sparqlString.toString();

        queryExecutor.beginRead();
        try {
            final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query);
            return queryExecution.execAsk();
        } finally {
            queryExecutor.end();
        }
    }

    @Override
    public Model getJenaModelForSubject(final URI namedGraphUri, final URI subject)
            throws NoSuchElementException {
        if (!namedGraphExists(namedGraphUri)) {
            throw new NoSuchElementException("namedGraph '" + namedGraphUri + "' is missing from the triplestore");
        }
        final Model model;
        model = modelFromQueryByUri(namedGraphUri, subject);
        if (model.isEmpty()) {
            throw new NoSuchElementException("resource '" + subject + "' is missing from the triplestore at namedGraph '"
                + namedGraphUri + "'");
        }
        return model;
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
                                               + namedGraph
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
                                               + namedGraph
                                               + " was missing from "
                                               + "the triplestore");
        }
    }

    @Override
    public <T extends IResource> List<T> getResources(final URI namedGraph, final Class<T> clazz, final String prefixes,
            final String where, final String searchTerms, final int limit, final int offset)
            throws StoreAccessException, ModelUnmarshallingException {
        return getResources(namedGraph, clazz, prefixes, where, searchTerms, limit, offset,
            null, null);
    }

    @Override
    public <T extends IResource> List<T> getResources(final URI namedGraph, final Class<T> clazz, final String prefixes,
            final String where, final String searchTerms, final int limit, final int offset,
            List<String> additionalDistinctVars, SelectBuilder additionalQueryFilter)
        throws StoreAccessException, ModelUnmarshallingException {

        String _prefixes = prefixes;
        String _where = where;

        _prefixes = (StringUtils.isNullOrEmpty(_prefixes) ? "" : _prefixes + ",") + oslcQueryPrefixes(clazz);
        _where = (StringUtils.isNullOrEmpty(_where) ? "" : _where + " and ") + oslcQueryWhere(clazz);
        Model model = getResources(namedGraph, _prefixes, _where, searchTerms, limit, offset, additionalDistinctVars,
            additionalQueryFilter);
        return getResourcesFromModel(model, clazz);
    }

    @Override
    public Model getResources(final URI namedGraph, final String prefixes, final String where, final int limit,
                              final int offset) {
        return getResources(namedGraph, prefixes, where, null, limit, offset);
    }

    @Override
    public Model getResources(final URI namedGraph, final String prefixes, final String where, final String searchTerms,
                              final int limit, final int offset) {
        return getResources(namedGraph, prefixes, where, searchTerms, limit, offset,
            null, null);
    }

    @Override
    public Model getResources(final URI namedGraph, final String prefixes, final String where, final String searchTerms,
                              final int limit, final int offset, List<String> additionalDistinctVars,
                              SelectBuilder additionalQueryFilter) {

        if (namedGraph != null) {
            //Make sure the designated namedGraph exists, if it is specified.
            //Otherwise, the search occurs across all named graphs.
            if (!namedGraphExists(namedGraph)) {
                throw new IllegalArgumentException("Named graph" + namedGraph + " was missing from the triplestore");
            }
        }

        SelectBuilder sparqlWhereQuery = constructSparqlWhere (prefixes, where, searchTerms, limit, offset,
            additionalDistinctVars, additionalQueryFilter);
        DescribeBuilder describeBuilder = new DescribeBuilder();
        describeBuilder.addVar("s")
        .addGraph((namedGraph != null) ? new ResourceImpl(String.valueOf(namedGraph)) : "?g", sparqlWhereQuery);

        if (null != additionalDistinctVars) {
            for (String additionalDistinctVar : additionalDistinctVars) {
                describeBuilder.addVar(additionalDistinctVar);
            }
        }

        Query describeQuery = describeBuilder.build() ;
        String describeQueryString = describeQuery.toString();
        Model execDescribe;
        queryExecutor.beginRead();
        try {
            final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(describeQueryString);

            try {
                log.trace("SPARQL Describe query for oslc.where='{}':\n{}", where, describeQueryString);
                Instant start = Instant.now();
                execDescribe = queryExecution.execDescribe();
                Instant finish = Instant.now();
                log.trace("GetResources - SPARQL Query Execution Duration: {} ms", Duration.between(start, finish).toMillis());
            } catch (RiotException e) {
                //a request that returns an empty set seems to cause an exception when using Marklogic.
                if ((e.getCause() == null) && (e.getMessage().equals("[line: 2, col: 2 ] Out of place: [DOT]"))) {
                    return ModelFactory.createDefaultModel();
                }
                // Otherwise, there is a proper exception that we need to deal with!
                throw e;
            }
        } finally {
            queryExecutor.end();
        }
        return execDescribe;
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
        queryExecutor.beginWrite();
        try {
            final UpdateProcessor up = queryExecutor.prepareSparqlUpdate(query.toString());
            up.execute();
            queryExecutor.commit();
        } finally {
            queryExecutor.end();
        }
    }

    @Override
    @Deprecated
    public Set<String> keySet() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void removeAll() {
        queryExecutor.prepareSparqlUpdate("CLEAR ALL").execute();
    }

    @Override
    public void close() {
        queryExecutor.release();
        log.debug("Underlying SPARQL connection has been released");
    }

    private <T extends IResource> String oslcQueryPrefixes(final Class<T> clazz) {
        return "rdf=" + "<" + org.apache.jena.vocabulary.RDF.uri + ">";
    }

    private <T extends IResource> String oslcQueryWhere(final Class<T> clazz) {
        return "rdf:type=" + "<" + getResourceNsUri(clazz) + ">";
    }

    private <T extends IResource> List<T> getResourcesFromModel(final Model model,
            final Class<T> clazz) throws ModelUnmarshallingException, StoreAccessException {
        try {
            Instant start = Instant.now();
            final Object[] obj = JenaModelHelper.fromJenaModel(model, clazz);
            @SuppressWarnings("unchecked") final T[] castObjects = (T[]) Array.newInstance(clazz,
                    obj.length);
            for (int i = 0; i < obj.length; i++) {
                castObjects[i] = clazz.cast(obj[i]);
            }
            Instant finish = Instant.now();
            log.trace("getResourcesFromModel - Execution Duration: {} ms", Duration.between(start, finish).toMillis());
            //The Model is most likely obtained via Select query that is orded by the subject (ascending)
            //See sparql construction in constructSparqlWhere()
            //Order the list below accordingly.
            return Arrays.stream(castObjects)
                .sorted(Comparator.comparing(IResource::getAbout)).collect(Collectors.toList());
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

        queryExecutor.beginRead();
        try {
            final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query.toString());
            return queryExecution.execDescribe();
        } finally {
            queryExecutor.end();
        }
    }

    private Model modelFromQueryByUri(final URI namedGraph, final URI uri) {
        final QuerySolutionMap map = getGraphMap(namedGraph);
        map.add("s", new ResourceImpl(String.valueOf(uri)));
        final String queryTemplate = "DESCRIBE ?s WHERE { GRAPH ?g { ?s ?p ?o . } }";
        final ParameterizedSparqlString query = new ParameterizedSparqlString(queryTemplate, map);

        Model execDescribe;
        queryExecutor.beginRead();
        try {
            String queryString = query.toString();
            final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(queryString);
            try {
                log.trace("SPARQL Describe query for uri='{}': \n{}", uri, queryString);
                Instant start = Instant.now();
                execDescribe = queryExecution.execDescribe();
                Instant finish = Instant.now();
                log.trace("GetResource - SPARQL Query Execution Duration: {} ms", Duration.between(start, finish).toMillis());
            } catch (RiotException e) {
                //a request that returns an empty set seems to cause an exception when using Marklogic.
                if ((e.getCause() == null) && (e.getMessage().equals("[line: 2, col: 2 ] Out of place: [DOT]"))) {
                    return ModelFactory.createDefaultModel();
                }
                // Otherwise, there is a proper exception that we need to deal with!
                throw e;
            }
        } finally {
            queryExecutor.end();
        }
        return execDescribe;

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
                                     + "      ORDER BY ASC(?s)\n"
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

        queryExecutor.beginRead();
        try {
            final QueryExecution queryExecution = queryExecutor.prepareSparqlQuery(query.toString());
            return queryExecution.execDescribe();
        } finally {
            queryExecutor.end();
        }
    }

    /**
     * This method currently only provides support for terms of type {@link Type#COMPARISON}, where the operator is
     * 'EQUALS', and the operand is either a {@link String} or a {@link URI}.
     */
    private SelectBuilder constructSparqlWhere(final String prefixes, final String where, final String searchTerms,
                                               final int limit, final int offset, List<String> additionalDistinctVars,
                                               SelectBuilder additionalQueryFilter) {

        SelectBuilder distinctResourcesQuery = new SelectBuilder();

        //Setup prefixes
        Map<String, String> prefixesMap = new HashMap<>();
        try {
            if (!StringUtils.isNullOrEmpty(prefixes)) {
                prefixesMap = QueryUtils.parsePrefixes(prefixes);
                for (Entry<String, String> prefix : prefixesMap.entrySet()) {
                    distinctResourcesQuery.addPrefix(prefix.getKey(), prefix.getValue());
                }
            }
        } catch (ParseException e) {
            throw new IllegalArgumentException("prefixesExpression could not be parsed", e);
        }

        distinctResourcesQuery
        .addVar( "s" )
        .setDistinct(true)
        .addWhere( "?s", "?p", "?o");

        if (null != additionalDistinctVars) {
            for (String additionalDistinctVar : additionalDistinctVars) {
                distinctResourcesQuery.addVar(additionalDistinctVar);
            }
        }
        if (null != additionalQueryFilter) {
            distinctResourcesQuery.addWhere(additionalQueryFilter);
        }

        //Setup where
        WhereClause whereClause = null;
        try {
            if (!StringUtils.isNullOrEmpty(where)) {
                whereClause = QueryUtils.parseWhere(where, prefixesMap);
                List<SimpleTerm> parseChildren = whereClause.children();
                for (SimpleTerm simpleTerm : parseChildren) {
                    Type termType = simpleTerm.type();
                    PName property = simpleTerm.property();

                    if (!termType.equals(Type.COMPARISON)) {
                        throw new UnsupportedOperationException("only support for terms of type Comparisons");
                    }
                    ComparisonTerm aComparisonTerm = (ComparisonTerm) simpleTerm;
                    if (!aComparisonTerm.operator().equals(Operator.EQUALS)) {
                        throw new UnsupportedOperationException(
                            "only support for terms of type Comparisons, where the operator is 'EQUALS'");
                    }

                    Value comparisonOperand = aComparisonTerm.operand();
                    Value.Type operandType = comparisonOperand.type();
                    String predicate;
                    if (property.local.equals("*")) {
                        predicate = "?p";
                    } else {
                        predicate = property.toString();
                    }

                    switch (operandType) {
                        case DECIMAL:
                            DecimalValue decimalOperand = (DecimalValue) comparisonOperand;
                            distinctResourcesQuery.addWhere("?s", predicate, decimalOperand.value());
                            break;
                        case STRING:
                            StringValue stringOperand = (StringValue) comparisonOperand;
                            distinctResourcesQuery.addWhere("?s", predicate, "\"" + stringOperand.value() + "\"");
                            break;
                        case URI_REF:
                            UriRefValue uriOperand = (UriRefValue) comparisonOperand;
                            distinctResourcesQuery.addWhere("?s", predicate, new ResourceImpl(uriOperand.value()));
                            break;
                        default:
                            throw new UnsupportedOperationException("only support for terms of type Comparisons," +
                                " where the operator is 'EQUALS', and the operand is either a String, an Integer or a URI");
                    }
                }
            }
        } catch (ParseException e) {
            throw new IllegalArgumentException("whereExpression could not be parsed", e);
        }

        //Setup searchTerms
        //Add a sparql filter "FILTER regex(?o, "<searchTerms>", "i")" to the distinctResourcesQuery
        if (!StringUtils.isNullOrEmpty(searchTerms)) {
            ExprFactory factory = new ExprFactory();
            E_Regex regex = factory.regex(factory.str("?o"), searchTerms, "i");
            distinctResourcesQuery.addFilter(regex);
        }


        if ((limit > 0 || offset > 0) && (! OSLC4JUtils.isLyoStorePagingUnsafe())) {
            distinctResourcesQuery.addOrderBy("?s", Order.ASCENDING);
        }

        if (limit > 0) {
            distinctResourcesQuery.setLimit(limit);
        }
        if (offset > 0) {
            distinctResourcesQuery.setOffset(offset);
        }

        SelectBuilder constructSelectQuery = new SelectBuilder();
        constructSelectQuery.addVar( "s p o" )
            .addSubQuery(distinctResourcesQuery);

        return constructSelectQuery;
    }

}
