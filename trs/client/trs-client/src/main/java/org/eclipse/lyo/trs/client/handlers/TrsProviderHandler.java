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
package org.eclipse.lyo.trs.client.handlers;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.trs.client.exceptions.ServerRollBackException;
import org.eclipse.lyo.trs.client.model.BaseMember;
import org.eclipse.lyo.trs.client.model.ChangeEventMessageTR;
import org.eclipse.lyo.trs.client.util.ITrackedResourceClient;
import org.eclipse.lyo.trs.client.util.ProviderUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base class for every TRS provider. Handles all periodic operations for a TRS
 * provider. This is the first version which was developed of a TRS provider
 * which does not support any multi threading of A TRS provider's opreations The
 * TRSProviderMultiThreaded class extends this class with support for
 * multithreading.
 *
 * @author Omar
 */
public class TrsProviderHandler implements IProviderHandler {
    private static final Logger log = LoggerFactory.getLogger(TrsProviderHandler.class);

    private final ITrackedResourceClient trsClient;
    private final IProviderEventHandler handler;

    /**
     * The URI of the last processed change event
     */
    private URI lastProcessedChangeEventUri;

    /**
     * The entry point URI for the tracked resource set of this provider
     */
    private URI trsUriBase;

    public TrsProviderHandler(
            URI trsUriBase,
            final ITrackedResourceClient trsClient,
            final IProviderEventHandler handler) {
        this.trsUriBase = trsUriBase;
        this.trsClient = trsClient;
        this.handler = handler;
    }

    @Override
    public String toString() {
        return "TrsProviderHandler{" + "trsUriBase='" + trsUriBase + '\'' + '}';
    }

    /**
     * Implementation of the method inherited from the TRSTaskHandler class. a
     * call to the periodic processing of the change events is done. If an
     * exception is thrown it's logged and the uri of the last processed change
     * event is set to null. This means that during the next period the
     * processing of the base will be done all over again
     */
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
     * Create the necessary sparql update for processing the change events and
     * send it to the sparql update service
     *
     * @param changeEvent the change event to be processed
     */
    private void processChangeEvent(ChangeEvent changeEvent) {
        URI changed = changeEvent.getChanged();
        log.info("processing resource " + changed.toString() + " change event ");

        Model trsResourceModel = null;
        if (!(changeEvent instanceof Deletion)) {
            trsResourceModel = trsClient.fetchTRSRemoteResource(changed);
        }

        final ChangeEventMessageTR eventMessageTR =
                new ChangeEventMessageTR(changeEvent, trsResourceModel);

        handler.handleChangeEvent(eventMessageTR);

        log.info("finished processing resource " + changed.toString() + " change event ");
    }

    /**
     * The main method for a TRS provider. This method consists on the periodic
     * process of processing the new change events since last time and the
     * processing of the base in case this is the first time the TRS provider
     * thread is ran
     */
    private void pollAndProcessChanges() {

        log.info("started dealing with TRS Provider: " + trsUriBase);

        TrackedResourceSet updatedTrs = trsClient.extractRemoteTrs(trsUriBase);
        boolean indexingStage = false;
        List<URI> baseMembers = new ArrayList<>();

        // TODO Andrew@2018-02-28: ensure indexing happens when none was made or cutoff is lost
        /*
        Basically, two things can happen that trigger full reindex:
        1. Some event was not processed successfully (current TRS consumer behaviour).
        2. We are so hopelessly behind we can't locate our last processed element in the changelog.
         */
        if (lastProcessedChangeEventUri == null) {
            // If it is the indexing phase retrieve the representation of the base
            List<Base> bases = trsClient.updateBases(updatedTrs);

            for (Base base : bases) {
                baseMembers.addAll(base.getMembers());
            }

            lastProcessedChangeEventUri = bases.get(0).getCutoffEvent();
            indexingStage = true;
        }

        // Retrieve all change log pages until the page containing the last processed change event
        List<ChangeLog> changeLogs = fetchUpdatedChangeLogs(updatedTrs);

        /* Optimize the list of changes by removing successive update / creation events for the
        same resource and overwriting update / creation events of a resource with more recent
        deletion events.
         */
        List<ChangeEvent> compressedChanges =
                ProviderUtil.optimizedChangesList(changeLogs, lastProcessedChangeEventUri);

        /*======================================================*
         COMMON CODE END (with concurrent TRS provider handler)
        *======================================================*/

        /* Andrew: why does indexing here happens AFTER the change events are processed WHILE the
        concurrent handler does it first (though the change handlers don't wait for the base to
        be updated, the ExecutorService is fired async there).
        */
        /*
         If it is the indexing stage, remove the resources for which the most
         recent change event has already been processed and then process the
         remaining members
        */
        if (indexingStage) {
            //            baseChangeEventsOptimization(compressedChanges, baseMembers);
            // FIXME Andrew@2018-02-28: the base resource gets lost at this stage
            // Andrew@2019-01-15: not sure if I registered any resource losses before
            baseMembers =
                    ProviderUtil.baseChangeEventsOptimizationSafe(compressedChanges, baseMembers);

            for (URI baseMemberUri : baseMembers) {
                log.debug("Fetching TRS base from {}", baseMemberUri);
                Model baseResourceModel = trsClient.fetchTRSRemoteResource(baseMemberUri);
                log.debug("Processing base member '{}' creation event", baseMemberUri);
                final BaseMember baseMember = new BaseMember(baseMemberUri, baseResourceModel);
                handler.handleBaseMember(baseMember);

                // actually it is possible to generate a Creation event per resource in base!
                log.trace("Finished processing base member '{}' creation event", baseMemberUri);
            }
        }

        for (ChangeEvent changeEvent : compressedChanges) {
            try {
                processChangeEvent(changeEvent);
                lastProcessedChangeEventUri = changeEvent.getAbout();
            } catch (Exception e) {
                log.error("Error processing {}: ", changeEvent, e);
                return;
            }
        }

        handler.finishCycle();
        log.info("finished dealing with TRS Provider: " + trsUriBase);
    }

    /**
     * remove from the URI list the resources for which an event is already present in the change
     * event list. Done to avoid processing base members uselessly
     *
     * @param compressedChangesList the optimized list of change events
     * @param baseMembers           the members of the base
     */
    @Deprecated
    protected void baseChangeEventsOptimization(
            List<ChangeEvent> compressedChangesList, List<URI> baseMembers) {
        for (ChangeEvent changeEvent : compressedChangesList) {
            URI changedResource = changeEvent.getChanged();
            if (baseMembers.contains(changedResource)) {
                log.debug(
                        "Removing '{}' from the base because it is already in the changelog",
                        changeEvent);
                baseMembers.remove(changedResource);
            }
        }
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
                    "The sync event can not be found. The sever provinding the trs at: "
                            + trsUriBase
                            + " seems to "
                            + "have been rollecd back to a previous "
                            + "state");
        }
        return changeLogs;
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
    private boolean fetchRemoteChangeLogs(ChangeLog currentChangeLog, List<ChangeLog> changeLogs) {
        boolean foundChangeEvent = false;
        URI previousChangeLog;
        do {
            if (currentChangeLog != null) {
                changeLogs.add(currentChangeLog);
                if (ProviderUtil.changeLogContainsEvent(
                        lastProcessedChangeEventUri, currentChangeLog)) {
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
}
