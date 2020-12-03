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
import java.util.Collections;
import java.util.Comparator;
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

    public static boolean isNilUri(URI currentPageUri) {
        return currentPageUri == null || currentPageUri.toString().equals(RDF.nil.getURI());
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
        firstChangelogEvents.sort(Comparator.comparing(ChangeEvent::getOrder));
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
        changesToProcess.sort(Comparator.comparing(ChangeEvent::getOrder));

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
            reducedChangesList.sort(Comparator.comparing(ChangeEvent::getOrder));
        } else {
            reducedChangesList = new ArrayList<>();
        }
        return reducedChangesList;
    }
}
