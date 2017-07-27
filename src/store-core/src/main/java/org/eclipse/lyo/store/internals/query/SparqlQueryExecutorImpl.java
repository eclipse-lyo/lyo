package org.eclipse.lyo.store.internals.query;

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

import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.update.UpdateExecutionFactory;
import com.hp.hpl.jena.update.UpdateFactory;
import com.hp.hpl.jena.update.UpdateProcessor;

/**
 * SparqlQueryExecutorImpl is a SPARQL endpoint-based implementation of {@link JenaQueryExecutor}.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public class SparqlQueryExecutorImpl implements JenaQueryExecutor {

    private final String queryEndpoint;
    private final String updateEndpoint;

    public SparqlQueryExecutorImpl(final String sparqlEndpoint, final String updateEndpoint) {
        this.queryEndpoint = sparqlEndpoint;
        this.updateEndpoint = updateEndpoint;
    }

    @Override
    public QueryExecution prepareSparqlQuery(final String query) {
        return QueryExecutionFactory.sparqlService(queryEndpoint, query);
    }

    @Override
    public UpdateProcessor prepareSparqlUpdate(final String query) {
        return UpdateExecutionFactory.createRemote(UpdateFactory.create(query), updateEndpoint);
    }
}
