/*
 * Copyright (c) 2019 KTH Royal Institute of Technology and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.trs.client.util;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.core.exception.LyoModelException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProviderUtil {
    private final static Logger log = LoggerFactory.getLogger(ProviderUtil.class);

    public static boolean isNotEmptySingletonArray(Object[] changeLogs) {
        return changeLogs != null && changeLogs.length == 1;
    }

    public static boolean isNotEmpty(Object[] deletions) {
        return deletions != null && deletions.length > 0;
    }

    /**
     * extract the change log projo from the rdf model of the change log
     * returned by the server
     *
     * @param rdFModel of thr change log
     *
     * @return change log pojo
     */
    public static ChangeLog extractChangeLogFromRdfModel(Model rdFModel) throws LyoModelException {
        log.debug("started extracting change log from rdf model");
        ChangeLog[] changeLogs;

        Modification[] modifications;
        Deletion[] deletions;
        Creation[] creations;

        changeLogs = JenaModelHelper.unmarshal(rdFModel, ChangeLog.class);
        creations = JenaModelHelper.unmarshal(rdFModel, Creation.class);
        modifications = JenaModelHelper.unmarshal(rdFModel, Modification.class);
        deletions = JenaModelHelper.unmarshal(rdFModel, Deletion.class);

        if (isNotEmptySingletonArray(changeLogs) && changeLogs[0] != null) {
            ChangeLog changeLog = changeLogs[0];
            changeLog.getChange().clear();
            if (isNotEmpty(modifications)) {
                changeLog.getChange().addAll(Arrays.asList((Modification[]) modifications));
            }

            if (isNotEmpty(creations)) {
                changeLog.getChange().addAll(Arrays.asList((Creation[]) creations));
            }

            if (isNotEmpty(deletions)) {
                changeLog.getChange().addAll(Arrays.asList((Deletion[]) deletions));
            }
            log.debug("finished extracting change log set from rdf model");
            return changeLog;
        } else {
            log.warn("the change log was missing; returning an empty one");
            return new ChangeLog();
        }
    }

    public static boolean isNilUri(URI currentPageUri) {
        return currentPageUri == null || currentPageUri.toString().equals(RDF.nil.getURI());
    }

    /**
     * extract the base projo from the rdf model of the base returned by the
     * server
     *
     * @param rdFModel of the base
     *
     * @return base pojo
     */
    public static Base extractBaseFromRdfModel(Model rdFModel) throws LyoModelException {
        log.debug("started extracting base from rdf model");
        Page nextPage;
        Base baseObj = null;
        Object[] nextPageArray;
        Object[] basesArray;
        nextPageArray = JenaModelHelper.unmarshal(rdFModel, Page.class);
        basesArray = JenaModelHelper.unmarshal(rdFModel, Base.class);

        if (isNotEmptySingletonArray(basesArray) && basesArray[0] instanceof Base) {
            baseObj = (Base) basesArray[0];
        }

        if (baseObj == null) {
            // FIXME Andrew@2019-07-15: nulls
            return null;
        }

        if (isNotEmptySingletonArray(nextPageArray) && nextPageArray[0] instanceof Page) {
            nextPage = (Page) nextPageArray[0];
            baseObj.setNextPage(nextPage);
            log.debug("finished extracting base from rdf model");
            return baseObj;
        }
        log.debug("finished extracting base from rdf model");
        // FIXME Andrew@2019-07-15: nulls
        return null;
    }

    /**
     * use osl4j functionality to retrieve a TRS object from the rdf model of
     * the TRS returned by the server
     *
     * @param rdFModel the rdf model
     *
     * @return the TRS pojo extracted from the TRS rdf model
     */
    public static TrackedResourceSet extractTrsFromRdfModel(Model rdFModel)
            throws LyoModelException {
        log.debug("started extracting tracked resource set from rdf model");

        TrackedResourceSet[] trackedResourceSets = JenaModelHelper.unmarshal(rdFModel,
                TrackedResourceSet.class);

        if (isNotEmptySingletonArray(trackedResourceSets) && trackedResourceSets[0] != null) {
            TrackedResourceSet trs = trackedResourceSets[0];
            ChangeLog trsChangeLog = extractChangeLogFromRdfModel(rdFModel);
            try {
                trs.setChangeLog(trsChangeLog);
            } catch (URISyntaxException e) {
                // TODO https://github.com/eclipse/lyo.core/issues/102
                throw new IllegalStateException("Should never happen");
            }
            log.debug("finished extracting tracked resource set from rdf model");
            return trs;
        }
        throw new IllegalArgumentException("TRS resource cannot be extracted from the Model");
    }

    /**
     * returns true if the change log pojo contains the change event with the
     * given uri and false otherwise
     */
    public static boolean changeLogContainsEvent(URI syncPointUri, ChangeLog changeLog) {
        for (ChangeEvent changeEvent : changeLog.getChange()) {
            if (changeEvent.getAbout().equals(syncPointUri)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 1. create an ordered list of change events from the list of change logs
     * given as an argument 2. Cut the list at the last processed change event
     * 3. Optimize the changes list by removing all redundant events for the
     * same resource
     *
     * @param changeLogs the list of change logs containing the change events to be
     *                   processed
     *
     * @return the optimized ordered list of change events
     */
    public static List<ChangeEvent> optimizedChangesList(List<ChangeLog> changeLogs,
            URI lastProcessedChangeEventUri) {
        Collections.reverse(changeLogs);

        ChangeLog firstChangeLog = changeLogs.get(0);
        List<ChangeEvent> firstChangelogEvents = firstChangeLog.getChange();

        // sort the events, needed for the cut-off later
        firstChangelogEvents.sort(new ChangeEventComparator());
        firstChangeLog.setChange(firstChangelogEvents);

        // TODO Andrew@2018-02-28: just delete this line, getter after setter is some superstition
//        firstChangelogEvents = firstChangeLog.getChange();

        // if the last processed event is in the CL, it must be in the first
        // see 'fetchUpdatedChangeLogs' for details why
        int indexOfSync = -1;
        for (ChangeEvent changeEvent : firstChangelogEvents) {
            if (changeEvent.getAbout().equals(lastProcessedChangeEventUri)) {
                indexOfSync = firstChangelogEvents.indexOf(changeEvent);
                break;
            }
        }

        firstChangelogEvents = firstChangelogEvents.subList(indexOfSync + 1,
                firstChangelogEvents.size());
        firstChangeLog.setChange(firstChangelogEvents);

        List<ChangeEvent> changesToProcess = new ArrayList<>();
        // merge all changelogs, events after the cutoff from the first log are there
        for (ChangeLog changeLog : changeLogs) {
            changesToProcess.addAll(changeLog.getChange());
        }
        changesToProcess.sort(new ChangeEventComparator());

        // NB! Andrew@2018-02-27: this is not going to work for getting all changes via MQTT embedding
        // TODO Andrew@2019-01-15: refactor to support MQTT
        // TODO Andrew@2018-02-27: output warning for the events we missed if compress eliminated anything

        // replace all change events for a single resource with the latest event only
        List<ChangeEvent> compressedChanges = compressChanges(changesToProcess);
        return compressedChanges;
    }

    public static List<URI> baseChangeEventsOptimizationSafe(
            List<ChangeEvent> compressedChangesList, List<URI> baseMembers) {
        // do it once to improve performance actually
        final Set<URI> compressedUris = compressedChangesList.stream()
                .map(AbstractResource::getAbout)
                .collect(Collectors.toSet());
        List<URI> filteredBase = new ArrayList<>(baseMembers.size());

        for (URI baseMember : baseMembers) {
            if (!compressedUris.contains(baseMember)) {
                filteredBase.add(baseMember);
            } else {
                log.debug("Removing {} from the base because it has been updated since in the " +
                        "changelog", baseMember);
            }
        }

        return filteredBase;
    }

    /**
     * takes an ordered list of change events to be processed and compressed the
     * list by removing multiple change events for the same resource and keeping
     * only the latest event for that resource
     *
     * @return an ordered optimized list of change events
     */
    private static List<ChangeEvent> compressChanges(List<ChangeEvent> changesToProcess) {
        Map<URI, ChangeEvent> resToChangeEventMap = new HashMap<>();

        for (ChangeEvent changeToProcess : changesToProcess) {
            resToChangeEventMap.put(changeToProcess.getChanged(), changeToProcess);
        }

        // TODO Andrew@2018-02-27: create compressed list in one go
        // Algorithm:
        // - use a hashset to keep track of the resource uris
        // - walk the changesToProcess in reverse
        // - reverse the returned list
        List<ChangeEvent> reducedChangesList;
        if (!resToChangeEventMap.isEmpty()) {
            reducedChangesList = new ArrayList<>(resToChangeEventMap.values());
            reducedChangesList.sort(new ChangeEventComparator());

        } else {
            reducedChangesList = new ArrayList<>();
        }
        return reducedChangesList;
    }
}
