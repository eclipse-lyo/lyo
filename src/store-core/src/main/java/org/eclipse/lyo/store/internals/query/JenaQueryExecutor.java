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
import com.hp.hpl.jena.update.UpdateProcessor;

/**
 * QueryExecutor is an interface that allows to run SPARQL queries on different triplestore
 * interfaces (network endpoints, Jena API, etc.).
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.14.0
 */
public interface JenaQueryExecutor {
    /**
     * Prepares a SPARQL Query executor (read-only).
     *
     * @param query SPARQL query string
     * @return prepared executor
     */
    QueryExecution prepareSparqlQuery(String query);

    /**
     * Prepares a SPARQL Update processor (write-only).
     *
     * @param query SPARQL query string
     * @return prepared processor
     */
    UpdateProcessor prepareSparqlUpdate(String query);
}
