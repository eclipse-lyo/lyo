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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

package org.eclipse.lyo.trs.client.handlers;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.trs.client.exceptions.RepresentationRetrievalException;
import org.eclipse.lyo.trs.client.exceptions.ServerRollBackException;
import org.eclipse.lyo.trs.client.model.BaseMember;
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR;
import org.eclipse.lyo.trs.client.util.ITrackedResourceClient;
import org.eclipse.lyo.trs.client.util.ProviderUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Specialization fothe TRS provider class which supports multithreading when it
 * comes to the processing of the base members and the change events.
 * Additionally, processes the sparql updates as a single transaction. In case
 * the sparql update transaction is not successful, then the indexing of the
 * base members is restarted all over again
 *
 * @author Omar
 */
public class ConcurrentTrsProviderHandler implements IProviderHandler {
    private final static Logger log = LoggerFactory.getLogger(ConcurrentTrsProviderHandler.class);
    private final URI trsUriBase;
    private final ITrackedResourceClient trsClient;
    private final IProviderEventHandler handler;
    private URI lastProcessedChangeEventUri;

    public ConcurrentTrsProviderHandler(URI trsUriBase, final ITrackedResourceClient trsClient,
            IProviderEventHandler handler) {
        this.trsUriBase = trsUriBase;
        this.trsClient = trsClient;
        this.handler = handler;
    }

    @Override
    public void update() {
        try {
            pollAndProcessChanges();
        } catch (Exception e) {
            // FIXME Andrew@2019-07-15: can get stuck in the loop
            log.warn("Force rebase");
            lastProcessedChangeEventUri = null;
            handler.rebase();
        }
    }

    /**
     * Request the pages of the change log from the TRS provider sequentially
     * through the traversal of the paging information until the last processed
     * change event is found. Add the fetched change logs to the changeLogs
     * argument. In case the last processed change event is found true is
     * returned otherwise false is returned
     *
     * @param currentChangeLog the first change log from which the next page will be
     *                         retrieved to retrieve the other pages of the change log
     * @param changeLogs       the list of change logs which will be filled with the pages of
     *                         the change log
     *
     * @return true if the last processed change event is found, false otherwise
     */
    public boolean fetchRemoteChangeLogs(ChangeLog currentChangeLog, List<ChangeLog> changeLogs) {
        boolean foundChangeEvent = false;
        URI previousChangeLog;
        do {
            if (currentChangeLog != null) {
                changeLogs.add(currentChangeLog);
                if (ProviderUtil.changeLogContainsEvent(lastProcessedChangeEventUri,
                        currentChangeLog)) {
                    foundChangeEvent = true;
                    break;
                }
                previousChangeLog = currentChangeLog.getPrevious();
                currentChangeLog = trsClient.fetchRemoteChangeLog(previousChangeLog);
            } else {
                break;
            }
        } while (!RDF.nil.getURI().equals(previousChangeLog.toString()));
        return foundChangeEvent;
    }

    private void pollAndProcessChanges() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
        Date processingDateStart = new Date();
        log.info("started dealing with TRS Provider: " + trsUriBase);

        TrackedResourceSet updatedTrs = trsClient.extractRemoteTrs(trsUriBase);
        boolean indexingStage = false;
        List<URI> baseMembers = new ArrayList<>();
        if (lastProcessedChangeEventUri == null) {
            log.debug("Indexing Stage.");
            log.debug("Requesting Base members from remote server");
            List<Base> bases = trsClient.updateBases(updatedTrs);
            log.debug("Base members retrieved !");
            for (Base base : bases) {
                baseMembers.addAll(base.getMembers());
            }

            lastProcessedChangeEventUri = bases.get(0).getCutoffEvent();
            indexingStage = true;
        }
        log.debug("Requesting changeLogs from Remote Server");
        List<ChangeLog> changeLogs = fetchUpdatedChangeLogs(updatedTrs);
        log.debug("change Logs Retrieved ! ");
        log.debug("Compressing the list of changes ! ");
        List<ChangeEvent> compressedChanges = ProviderUtil.optimizedChangesList(changeLogs,
                lastProcessedChangeEventUri);
        log.debug("Change list compressed ! ");

        /*======================================================*
          COMMON CODE END (with TRS provider handler)
         *======================================================*/

        log.debug("starting the processing of change events and base members creations");

        log.trace("Creating necessary sparql update queries");

        ExecutorService handlerExecutor = Executors.newCachedThreadPool();

        if (indexingStage) {
            log.debug("optimizing the list of base members against the change events to be " +
                    "processed.");
            baseMembers = ProviderUtil.baseChangeEventsOptimizationSafe(compressedChanges,
                    baseMembers);
            log.debug("finished optimizing the list of base members against the change events to " +
                    "" + "" + "" + "be" + " processed !");
            log.debug("Indexing stage. Base members creations will be be added to the list of " +
                    "events to be processed.");

            for (URI baseMemberUri : baseMembers) {
                handlerExecutor.execute(() -> {
                    try {
                        Model graphToUpload = trsClient.fetchTRSRemoteResource(baseMemberUri);
                        final BaseMember baseMember = new BaseMember(baseMemberUri, graphToUpload);
                        handler.handleBaseMember(baseMember);
                    } catch (RepresentationRetrievalException e) {
                        log.warn("Failed to retrieve {}", baseMemberUri);
                    }
                });
            }
        }

        for (ChangeEvent compressedChangeEvent : compressedChanges) {
            handlerExecutor.execute(() -> {
                final ChangeEventMessageTR eventMessageTR = new ChangeEventMessageTR(
                        compressedChangeEvent, null);
                handler.handleChangeEvent(eventMessageTR);
            });
            lastProcessedChangeEventUri = compressedChangeEvent.getAbout();
        }

        handlerExecutor.shutdown();

        if (!handlerExecutor.isTerminated()) {
            try {
                handlerExecutor.awaitTermination(3000, TimeUnit.MILLISECONDS);
                handlerExecutor.shutdownNow();
            } catch (InterruptedException e) {
                log.debug("Handler thread interrupted while awaiting executor termination", e);
            }
        }

        handler.finishCycle();
        Date finishProcessingData = new Date();
        log.info("finished dealing with TRS Provider: " + trsUriBase);
        log.debug("start dealing at: " + sdf.format(processingDateStart) + " . Finished dealing " +
                "with provider at: " + sdf.format(finishProcessingData));
    }

    /**
     * Return a list of change Lo objects corresponding to the pages of the
     * change log after requesting them from the change log url. The pages of
     * the change log will be requested using the change log segmentation until
     * the last change event which was processed is found. The change log url is
     * retrieved from the trs object passes as a parameter.
     *
     * @param updatedTrs the trs object retrieved after retrieving it using the trs uri
     *
     * @return the pages of the change log of this trs provider
     */
    private List<ChangeLog> fetchUpdatedChangeLogs(TrackedResourceSet updatedTrs) {

        ChangeLog firstChangeLog = updatedTrs.getChangeLog();
        List<ChangeLog> changeLogs = new ArrayList<>();
        boolean foundSyncEvent;

        foundSyncEvent = fetchRemoteChangeLogs(firstChangeLog, changeLogs);
        if (!foundSyncEvent) {
            lastProcessedChangeEventUri = null;
            throw new ServerRollBackException(
                    "The sync event can not be found. The sever provinding the trs at: " +
                            trsUriBase + " seems to " + "have been rollecd back to a previous " +
                            "state");
        }
        return changeLogs;
    }

}
