/**
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.consumer.TRSProvider.handler;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicLong;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.log4j.Logger;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.trs.consumer.RepresentationRetrievalException;
import org.eclipse.lyo.oslc4j.trs.consumer.ServerRollBackException;
import org.eclipse.lyo.oslc4j.trs.consumer.httpclient.TRSHttpClient;
import org.eclipse.lyo.oslc4j.trs.consumer.sparql.SparqlUtil;

import net.oauth.OAuthException;

/**
 * Specialization fothe TRS provider class which supports multithreading when it
 * comes to the processing of the base members and the change events.
 * Additionally, processes the sparql updates as a single transaction. In case
 * the sparql update transaction is not successful, then the indexing of the
 * base members is restarted all over again
 *
 * @author Omar
 *
 */
public class ConcurrentTRSProviderHandler extends TrsProviderHandler {
    public ConcurrentTRSProviderHandler(String trsUriBase, String sparqlQueryService, String sparqlUpdateService,
            TRSHttpClient trsHttpClient, String userName, String pwd, String sparql_user, String sparql_pwd) {
        super(trsUriBase, sparqlQueryService, sparqlUpdateService, trsHttpClient, userName, pwd, sparql_user,
                sparql_pwd);

    }

    public ConcurrentTRSProviderHandler(String trsUriBase, String sparqlQueryService, String sparqlUpdateService,
            TRSHttpClient trsHttpClient, String userName, String pwd) {
        super(trsUriBase, sparqlQueryService, sparqlUpdateService, trsHttpClient, userName, pwd);

    }

    final static Logger logger = Logger.getLogger(ConcurrentTRSProviderHandler.class);

    private void indexingStage(List<ChangeEvent> compressedChanges,
            ExecutorService changeEventsAndBaseProcessingExecutor, List<String> queries, AtomicLong modelSize,
            List<URI> baseMembers)
                    throws IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
                    SecurityException, NoSuchMethodException, IOException, OAuthException, URISyntaxException,
                    DatatypeConfigurationException, OslcCoreApplicationException {

        logger.debug("optimizing the list of base members against the change events to be processed.");
        baseChangeEventsOptimization(compressedChanges, baseMembers);
        logger.debug("finished optimizing the list of base members against the change events to be processed !");
        logger.debug("Indexing stage. Base members creations will be be added to the list of events to be processed.");

        for (URI baseMemberUri : baseMembers) {
            BaseMemberHandler baseMemberHandler = new BaseMemberHandler(oslcClient, sparqlQueryService,
                    sparqlUpdateService, baseAuth_userName, baseAuth_pwd, baseMemberUri.toString(), queries, modelSize);
            changeEventsAndBaseProcessingExecutor.execute(baseMemberHandler);
        }

    }

    public void pollAndProcessChanges2() throws IOException, OAuthException, URISyntaxException,
    ServerRollBackException, IllegalAccessException, IllegalArgumentException, InstantiationException,
    InvocationTargetException, SecurityException, NoSuchMethodException, DatatypeConfigurationException,
    OslcCoreApplicationException, RepresentationRetrievalException {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        Date processingDateStart = new Date();

        ExecutorService changeEventsAndBaseProcessingExecutor = null;

        List<String> queries = new ArrayList<>();
        AtomicLong modelSize = new AtomicLong(0);

        changeEventsAndBaseProcessingExecutor = (ThreadPoolExecutor) Executors.newCachedThreadPool();

        logger.info("started dealing with TRS Provider: " + trsUriBase);

        TrackedResourceSet updatedTrs = extractRemoteTrs();
        logger.debug("Requesting changeLogs from Remote Server");

        List<URI> baseMembers = new ArrayList<URI>();
        boolean isIndexingStage = false;
        if (lastProcessedChangeEventUri == null) {
            logger.debug("Indexing Stage.");
            logger.debug("Requesting Base members from remote server");
            List<Base> bases = updateBases(updatedTrs);
            logger.debug("Base members retrieved !");
            for (Base base : bases) {
                baseMembers.addAll(base.getMembers());
            }

            lastProcessedChangeEventUri = bases.get(0).getCutoffEvent();
            isIndexingStage = true;
        }

        List<ChangeLog> changeLogs = fetchUpdatedChangeLogs(updatedTrs);
        logger.debug("change Logs Retrieved ! ");
        logger.debug("Compressing the list of changes ! ");
        List<ChangeEvent> compressedChanges = optimizedChangesList(changeLogs);
        logger.debug("Change list compressed ! ");

        if (isIndexingStage)
            indexingStage(compressedChanges, changeEventsAndBaseProcessingExecutor, queries, modelSize, baseMembers);

        logger.debug("starting the processing of change events and base members creations");

        logger.debug("Creating necessary sparql update queries");

        for (ChangeEvent compressedChangeEvent : compressedChanges) {
            ChangeEventHandler changeEventHandler = new ChangeEventHandler(oslcClient, sparqlQueryService,
                    sparqlUpdateService, baseAuth_userName, baseAuth_pwd, compressedChangeEvent, queries, modelSize);
            changeEventsAndBaseProcessingExecutor.execute(changeEventHandler);
            lastProcessedChangeEventUri = compressedChangeEvent.getAbout();
        }

        changeEventsAndBaseProcessingExecutor.shutdown();

        while (!changeEventsAndBaseProcessingExecutor.isTerminated()) {

        }

        if (!queries.isEmpty()) {
            queries.contains(null);
            if (queries.contains("")) {
                lastProcessedChangeEventUri = null;
                throw new RepresentationRetrievalException();
            }
            logger.debug("number of processed queries: " + queries.size());
            logger.debug("finished Creating necessary sparql update queries");
            StringBuilder queriesStringBuilder = new StringBuilder();

            for (String query : queries) {
                queriesStringBuilder.append(query);
                queriesStringBuilder.append("; \n");
            }

            queriesStringBuilder.replace(queriesStringBuilder.lastIndexOf("; \n"),
                    queriesStringBuilder.lastIndexOf("; \n") + 1, "");

            String finalQueryString = queriesStringBuilder.toString();
            logger.debug(finalQueryString);
            logger.debug("a total of: " + modelSize + " triple . In the sparql update query");
            logger.debug("sending Update SPARQL Query to server");

            SparqlUtil.processQuery_sesame(finalQueryString, sparqlUpdateService, sparql_baseAuth_userName,
                    sparql_baseAuth_pwd);
            logger.debug("Update SPARQL Queries successful!");
        }

        Date finishProcessingData = new Date();

        logger.info("finished dealing with TRS Provider: " + trsUriBase);
        logger.debug("start dealing at: " + sdf.format(processingDateStart) + " . Finished dealing with provider at: "
                + sdf.format(finishProcessingData));

    }

    @Override
    public void pollAndProcessChanges() throws IOException, OAuthException, URISyntaxException, ServerRollBackException,
    IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
    SecurityException, NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException,
    RepresentationRetrievalException {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        Date processingDateStart = new Date();
        logger.info("started dealing with TRS Provider: " + trsUriBase);

        TrackedResourceSet updatedTrs = extractRemoteTrs();
        boolean indexingStage = false;
        List<URI> baseMembers = new ArrayList<URI>();
        if (lastProcessedChangeEventUri == null) {
            logger.debug("Indexing Stage.");
            logger.debug("Requesting Base members from remote server");
            List<Base> bases = updateBases(updatedTrs);
            logger.debug("Base members retrieved !");
            for (Base base : bases) {
                baseMembers.addAll(base.getMembers());
            }

            lastProcessedChangeEventUri = bases.get(0).getCutoffEvent();
            indexingStage = true;
        }
        logger.debug("Requesting changeLogs from Remote Server");
        List<ChangeLog> changeLogs = fetchUpdatedChangeLogs(updatedTrs);
        logger.debug("change Logs Retrieved ! ");
        logger.debug("Compressing the list of changes ! ");
        List<ChangeEvent> compressedChanges = optimizedChangesList(changeLogs);
        logger.debug("Change list compressed ! ");

        logger.debug("starting the processing of change events and base members creations");

        logger.debug("Creating necessary sparql update queries");

        ExecutorService changeEventsAndBaseProcessingExecutor = null;
        List<String> queries = new ArrayList<>();
        AtomicLong modelSize = new AtomicLong(0);
        changeEventsAndBaseProcessingExecutor = (ThreadPoolExecutor) Executors.newCachedThreadPool();

        if (indexingStage) {
            logger.debug("optimizing the list of base members against the change events to be processed.");
            baseChangeEventsOptimization(compressedChanges, baseMembers);
            logger.debug("finished optimizing the list of base members against the change events to be processed !");
            logger.debug(
                    "Indexing stage. Base members creations will be be added to the list of events to be processed.");

            for (URI baseMemberUri : baseMembers) {
                BaseMemberHandler baseMemberHandler = new BaseMemberHandler(oslcClient, sparqlQueryService,
                        sparqlUpdateService, baseAuth_userName, baseAuth_pwd, baseMemberUri.toString(), queries,
                        modelSize);
                changeEventsAndBaseProcessingExecutor.execute(baseMemberHandler);
            }
        }

        for (ChangeEvent compressedChangeEvent : compressedChanges) {
            ChangeEventHandler changeEventHandler = new ChangeEventHandler(oslcClient, sparqlQueryService,
                    sparqlUpdateService, baseAuth_userName, baseAuth_pwd, compressedChangeEvent, queries, modelSize);
            changeEventsAndBaseProcessingExecutor.execute(changeEventHandler);
            lastProcessedChangeEventUri = compressedChangeEvent.getAbout();
        }

        changeEventsAndBaseProcessingExecutor.shutdown();

        while (!changeEventsAndBaseProcessingExecutor.isTerminated()) {

        }

        if (!queries.isEmpty()) {
            queries.contains(null);
            if (queries.contains("")) {
                lastProcessedChangeEventUri = null;
                throw new RepresentationRetrievalException();
            }
            logger.debug("number of processed queries: " + queries.size());
            logger.debug("finished Creating necessary sparql update queries");
            StringBuilder queriesStringBuilder = new StringBuilder();

            for (String query : queries) {
                queriesStringBuilder.append(query);
                queriesStringBuilder.append("; \n");
            }

            queriesStringBuilder.replace(queriesStringBuilder.lastIndexOf("; \n"),
                    queriesStringBuilder.lastIndexOf("; \n") + 1, "");

            String finalQueryString = queriesStringBuilder.toString();
            logger.debug(finalQueryString);
            logger.debug("a total of: " + modelSize + " triple . In the sparql update query");
            logger.debug("sending Update SPARQL Query to server");

            SparqlUtil.processQuery_sesame(finalQueryString, sparqlUpdateService, sparql_baseAuth_userName,
                    sparql_baseAuth_pwd);
            logger.debug("Update SPARQL Queries successful!");
        }

        Date finishProcessingData = new Date();

        logger.info("finished dealing with TRS Provider: " + trsUriBase);
        logger.debug("start dealing at: " + sdf.format(processingDateStart) + " . Finished dealing with provider at: "
                + sdf.format(finishProcessingData));

    }
}
