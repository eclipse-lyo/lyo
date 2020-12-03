package org.eclipse.lyo.store.internals.query;

/*-
 * #%L
 * Contributors:
 *     Andrew Berezovskyi - initial implementation
 * %%
 * Copyright (C) 2016 - 2018 KTH Royal Institute of Technology
 * %%
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * #L%
 */

import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;

/**
 * SparqlQueryExecutorImpl is a SPARQL endpoint-based implementation of {@link JenaQueryExecutor}.
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public class SparqlQueryExecutorBasicAuthImpl implements JenaQueryExecutor {

    private final String queryEndpoint;
    private final String updateEndpoint;
    private final CloseableHttpClient client;

    public SparqlQueryExecutorBasicAuthImpl(final String sparqlEndpoint,
            final String updateEndpoint, final String login, final String password) {
        this.queryEndpoint = sparqlEndpoint;
        this.updateEndpoint = updateEndpoint;
        CredentialsProvider provider = new BasicCredentialsProvider();
        UsernamePasswordCredentials credentials = new UsernamePasswordCredentials(login, password);
        provider.setCredentials(AuthScope.ANY, credentials);

        client = HttpClientBuilder.create().setDefaultCredentialsProvider(provider).build();
    }

    @Override
    public QueryExecution prepareSparqlQuery(final String query) {
        return QueryExecutionFactory.sparqlService(queryEndpoint, query, client);
    }

    @Override
    public UpdateProcessor prepareSparqlUpdate(final String query) {
        return UpdateExecutionFactory.createRemote(
                UpdateFactory.create(query),
                updateEndpoint,
                client
        );
    }
}
