package org.eclipse.lyo.trs.client.handlers.sparql;

import java.util.ArrayList;
import java.util.List;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.store.Store;
import org.eclipse.lyo.store.StoreFactory;
import org.eclipse.lyo.trs.client.handlers.IProviderEventHandler;
import org.eclipse.lyo.trs.client.model.BaseMember;
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR;
import org.eclipse.lyo.trs.client.util.SparqlUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SparqlBatchingHandler implements IProviderEventHandler {
    private final static Logger log = LoggerFactory.getLogger(
            SparqlBatchingHandler.class);

    private final List<String> queries = new ArrayList<>();
    private final String sparqlUpdateService;
    private final String sparql_baseAuth_userName;
    private final String sparql_baseAuth_pwd;

    public SparqlBatchingHandler(final String sparqlUpdateService,
            final String sparql_baseAuth_userName, final String sparql_baseAuth_pwd) {
        this.sparqlUpdateService = sparqlUpdateService;
        this.sparql_baseAuth_userName = sparql_baseAuth_userName;
        this.sparql_baseAuth_pwd = sparql_baseAuth_pwd;
    }

    @Override
    public void finishCycle() {
        log.debug("number of processed queries: " + queries.size());
        String updateQuery = buildYugeQuery(queries);
        log.debug("sending Update SPARQL Query to server");

        // TODO: build one or use a pool
        Store store = StoreFactory.sparql(null, sparqlUpdateService,
            sparql_baseAuth_userName, sparql_baseAuth_pwd);
        try {
            store.rawUpdateQuery(updateQuery);
        } finally {
            store.close();
        }

        log.debug("Update SPARQL Queries successful!");

        queries.clear();
    }

    @Override
    public void handleBaseMember(final BaseMember baseMember) {
        StringBuilder query = new StringBuilder();
        String graphCreationQuery = SparqlUtil.createGraphQuery(baseMember.getUri());
        String addTriplesToGraphQuery = SparqlUtil.addTriplesToGraphQuery(baseMember.getUri(),
                baseMember.getModel());
        query.append(graphCreationQuery);
        query.append("; \n");
        query.append(addTriplesToGraphQuery);
        queries.add(query.toString());
    }

    @Override
    public void handleChangeEvent(final ChangeEventMessageTR eventMessageTR) {
        final ChangeEvent event = eventMessageTR.getChangeEvent();
        log.debug(
                "creating query for resource " + event.getChanged().toString() + " change event ");
        if (event instanceof Deletion) {
            String query = SparqlUtil.getChangeEventQuery(event, null);
            queries.add(query);
        } else {
            String query = SparqlUtil.getChangeEventQuery(event,
                    eventMessageTR.getTrackedResourceModel());
            queries.add(query);
        }
    }

    @Override
    public void rebase() {
        log.warn("Rebase");
    }

    private String buildYugeQuery(final List<String> queries) {
        StringBuilder queriesStringBuilder = new StringBuilder();

        for (String query : queries) {
            queriesStringBuilder.append(query);
            queriesStringBuilder.append("; \n");
        }

//          TODO  simply join instead of append - or check for the last element
        queriesStringBuilder.replace(queriesStringBuilder.lastIndexOf("; \n"),
                queriesStringBuilder.lastIndexOf("; \n") + 1, "");

        // TODO Andrew@2018-02-28: this is a YUGE query that can crash everything
        // I think individual queries are better executed in the handlers
        String finalQueryString = queriesStringBuilder.toString();
        log.debug(finalQueryString);
        return finalQueryString;
    }
}
