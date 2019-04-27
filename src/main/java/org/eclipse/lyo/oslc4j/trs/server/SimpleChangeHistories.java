/*-
 * Copyright (c) 2012, 2014  IBM Corporation
 * 2016-2017   KTH Royal Institute of Technology.
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
 * Hirotaka Matsumoto  -  Initial implementation
 * Omar Kacimi         -  Library conversion
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.UriBuilder;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TRSConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.trs.server.service.TrackedResourceSetService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is the backbone of the Tracked resource set service class. This class is extended by
 * oslc adapters wishing to implement an OSLC TRS interface. The implementing classes need to
 * implement one method returning the change history of the resources living in the tool exposed by
 * the OSLC adapter
 *
 * @version $version-stub$
 * @since 2.3.0
 */
public abstract class SimpleChangeHistories implements ChangeHistories {

    private final static Logger log = LoggerFactory.getLogger(SimpleChangeHistories.class);

    /**
     * Comparators for change events and history data
     */
    private static Comparator<HistoryData> histDataComparator = (object1, object2) -> {
        return object2.getTimestamp().compareTo(object1.getTimestamp()); // reverse
    };
    private static Comparator<ChangeEvent> changeEventComparator = (object1, object2) -> {
        return object2.getOrder() - object1.getOrder();
    };
    private final Object lockObject = new Object();
    /**
     * Page Limit for Base Resources
     */
    private final int BASE_PAGELIMIT = 40;
    /**
     * Page Limit for ChangeLogs
     */
    private final int CHANGELOGS_PAGELIMIT = 40;
//    protected             ArtificialTRSMaker      artificialTRSMaker    = new ArtificialTRSMaker();
    /**
     * Saved History data
     */

    protected List<HistoryData> allHistories = new ArrayList<>();
    /**
     * A flag which should be turned on to true in case the inheriting class will be exposing
     * artificial TRS data which using the utility method of the ArtificialTRSMaker class.
     */
    @Deprecated protected boolean artificialTRS = false;
    /**
     * If this flag is turned on, artificial history data will be periodically added for the
     * existing base resources
     */
    @Deprecated protected boolean periodicArtificialTRS = false;
    /**
     * This flag should be set to true by the implementing class in case it generates TRS data for
     * its TRS interface using the artifical TRS maker class functionality.
     */
    @Deprecated protected boolean hasBeenTweaked = false;
    /**
     * url prefix for the trs url e.g http://host/services/ will imply the trs uri shall be of the
     * type http://host/services/trs
     */
    @Deprecated protected String serviceBase;
    /**
     * interval for updating the base resources, in ms
     */
    private long baseUpdateInterval;
    private Date mostRecentChangeLogDate;
    private Date lastBaseResourceUpdatedDate;
    /**
     * List of base resources
     */
    private Map<String, Base> baseResouces;
    /**
     * List of Change Logs
     */
    private Map<String, ChangeLog> changeLogs;
    /**
     * Saved History
     */
    private HistoryData[] prevHistories;

    @Deprecated
    public SimpleChangeHistories(long baseUpdateInterval, String serviceBase) {
        this.baseUpdateInterval = baseUpdateInterval;
        this.serviceBase = serviceBase;
    }

    /**
     * @param baseUpdateInterval interval between base rebuilds, in milliseconds
     */
    public SimpleChangeHistories(long baseUpdateInterval) {
        this.baseUpdateInterval = baseUpdateInterval;
        this.serviceBase = null;
    }

    public void updateHistories(String changeType, List<URI> changedUris) {
        List<HistoryData> hd = new ArrayList<>();
        for (URI changedUri : changedUris) {
            HistoryData histData = HistoryData.getInstance(new Date(), changedUri, changeType);
            hd.add(histData);
        }

        updateHistories(hd);
    }

    @Override
    public void updateHistories(List<HistoryData> hd) {
        allHistories.addAll(hd);
        Collections.sort(allHistories, histDataComparator);
    }

    // protected abstract void init();

    @Override
    public HistoryData[] getOrderedHistory(Date dateAfter) {
        HistoryData[] unsortedHd = getHistory(dateAfter);
        Arrays.sort(unsortedHd, histDataComparator);
        return unsortedHd;
    }

    public void buildBaseResourcesAndChangeLogs(HttpServletRequest httpServletRequest)
            throws URISyntaxException {
        synchronized (lockObject) {
            buildBaseResourcesAndChangeLogsInternal();
        }
    }

    @Override
    public Base getBaseResource(String pagenum) {
        synchronized (lockObject) {
            buildBaseResourcesAndChangeLogsInternal();
            return baseResouces != null ? baseResouces.get(pagenum) : null;
        }
    }

    @Override
    public ChangeLog getChangeLog(String pagenum) {
        synchronized (lockObject) {
            buildBaseResourcesAndChangeLogsInternal();
            // changeLogs might be null
            return changeLogs != null ? changeLogs.get(pagenum) : null;
        }
    }

    public long getBaseUpdateInterval() {
        return baseUpdateInterval;
    }

    public void setBaseUpdateInterval(long uPDATEINTERVAL) {
        baseUpdateInterval = uPDATEINTERVAL;
    }

    private UriBuilder getTrsUriBuilder() {
        return UriBuilder.fromUri(OSLC4JUtils.getServletURI()).path("trs");
    }

    /**
     * Create the page attribute of the base object
     *
     * @param base        the base object for which this is the page
     * @param basePagenum number of the page
     *
     * @return the page of the base
     */
    private Page createNewBasePage(Base base, int basePagenum) {
        Page ldp = new Page();
        ldp.setAbout(getTrsUriBuilder().path(TRSConstants.TRS_TERM_BASE)
                .path(String.valueOf(basePagenum))
                .build());
        ldp.setNextPage(URI.create(TRSConstants.RDF_NIL));
        ldp.setPageOf(base);
        return ldp;
    }

    protected void newChangeEvent(final ChangeEvent ce) {}

    /**
     * Create a new page object with the page basepagenum
     *
     * @param basePagenum the number of this base page
     *
     * @return the created base page object
     */
    private Base createNewBase(int basePagenum) {
        Base base = new Base();
        base.setAbout(getTrsUriBuilder().path(TRSConstants.TRS_TERM_BASE).build());

        base.setNextPage(createNewBasePage(base, basePagenum));
        return base;
    }

    /**
     * Main method called to create the change log and the base objects when a call to the TRS
     * service is made. Depending on the refresh rate defined for the trs service, the base objects
     * might be completely rebuilt or only the change logs will be rebuilt
     */
    private void buildBaseResourcesAndChangeLogsInternal() {
        log.info("started building the change log and the base");

        Date nowDate = new Date();
        if ((lastBaseResourceUpdatedDate != null) && (baseUpdateInterval != -1) &&
                (nowDate.getTime() - lastBaseResourceUpdatedDate.getTime() > baseUpdateInterval)) {
            mostRecentChangeLogDate = null; // enforce to build all
        }
        boolean buildAll = ((mostRecentChangeLogDate == null) || (baseResouces == null));
        HistoryData[] updatedHistories = null;
        if (!buildAll) {
            // Get only updated Histories
            updatedHistories = getOrderedHistory(mostRecentChangeLogDate);
            if ((updatedHistories == null) || (updatedHistories.length == 0)) {
                return;
            }
        } else {

            log.info("Rebuild Base and ChangeLogs");
            baseResouces = null;
            mostRecentChangeLogDate = null;
            lastBaseResourceUpdatedDate = nowDate;
        }

        changeLogs = null;

        int basePagenum = 1;
        int changeLogPageNum = 1;

        URI nilURI = URI.create(TRSConstants.RDF_NIL);

        // even if no change logs, Base is necessary
        Base base = null;
        Base prevBase = null;
        if (buildAll) {
            base = createNewBase(basePagenum);
            baseResouces = new HashMap<>();
            baseResouces.put(String.valueOf(basePagenum), base);
        }

        // Try to get histories now.
        HistoryData[] histories = null;
        if (buildAll) {
            histories = getOrderedHistory(null);
            if (histories.length == 0) {
                return;
            }
        } else {
            int newSize = prevHistories != null ? prevHistories.length : 0;
            newSize += updatedHistories.length;
            histories = new HistoryData[newSize];
            int start = 0;
            System.arraycopy(updatedHistories, 0, histories, start, updatedHistories.length);
            start = updatedHistories.length;
            if ((prevHistories != null) && (prevHistories.length > 0)) {
                System.arraycopy(prevHistories, 0, histories, start, prevHistories.length);
                start = prevHistories.length;
            }
        }
        prevHistories = new HistoryData[histories.length];
        System.arraycopy(histories, 0, prevHistories, 0, histories.length);

        ChangeLog changeLog = null;
        ChangeLog prevChangeLog = null;

        int changeOrder = histories.length;
        int currentNumberOfMember = 0;
        int currentNumberOfChangeLog = 0;
        URI mostRecentChangeEventURI = null;
        List<URI> allmembers = buildAll ? new ArrayList<>(histories.length) : null;
        List<URI> deletedMembers = new ArrayList<>();
        for (HistoryData historyData : histories) {
            URI uri = historyData.getUri();

            String changedUriTemplate = "urn:urn-3:cm1.example.com:" +
                    TRSUtil.XSD_DATETIME_FORMAT.format(historyData.getTimestamp()) + ":" +
                    changeOrder;
            URI changedUri = URI.create(changedUriTemplate);
            String histDataType = historyData.getType();

            ChangeEvent ce = null;

            if (Objects.equals(histDataType, HistoryData.CREATED)) {
                ce = new Creation(changedUri, uri, changeOrder);
            } else if (Objects.equals(histDataType, HistoryData.MODIFIED)) {
                ce = new Modification(changedUri, uri, changeOrder);
            } else if (Objects.equals(histDataType, HistoryData.DELETED)) {
                ce = new Deletion(changedUri, uri, changeOrder);
                deletedMembers.add(uri);
            } else {
                log.error("Change Event {} has unknown kind: {}", uri, histDataType);
                throw new IllegalStateException();
            }

            if (mostRecentChangeEventURI == null) {
                mostRecentChangeEventURI = changedUri;
                mostRecentChangeLogDate = historyData.getTimestamp();
            }

            /*
             * create a new change log in case the number of changes on the
             * pages of the previous change log page has been reached. iterate
             * the number of pages and add reference from the new change log to
             * the previous one
             */
            if (changeLog == null) {
                URI prevPage;
                if (changeLogs == null) {
                    prevPage = getTrsUriBuilder().path(TRSConstants.TRS_TERM_CHANGE_LOG).build();
                } else {
                    prevPage = getTrsUriBuilder().path(TRSConstants.TRS_TERM_CHANGE_LOG)
                            .path(String.valueOf(changeLogPageNum + 1))
                            .build();
                }
                if (prevChangeLog != null) {
                    prevChangeLog.setPrevious(prevPage);
                    prevChangeLog = null;
                }
                if (changeLogs != null) {
                    changeLogPageNum++;
                }
                changeLog = new ChangeLog();
                changeLog.setAbout(prevPage);
                changeLog.setPrevious(nilURI);
                if (changeLogs == null) {
                    changeLogs = new HashMap<>();
                }
                changeLogs.put(String.valueOf(changeLogPageNum), changeLog);
            }

            changeLog.getChange().add(ce);
            currentNumberOfChangeLog++;

            if (buildAll && !allmembers.contains(uri) && !deletedMembers.contains(uri)) {
                // Base Page
                if (base == null) {
                    if (prevBase != null) {
                        URI nextPage = getTrsUriBuilder().path(TRSConstants.TRS_TERM_BASE)
                                .path(String.valueOf(basePagenum + 1))
                                .build();
                        prevBase.getNextPage().setNextPage(nextPage);
                        prevBase = null;
                    }
                    basePagenum++;
                    base = createNewBase(basePagenum);
                    baseResouces.put(String.valueOf(basePagenum), base);
                }
                base.getMembers().add(uri);// rdfs:member is mandatory
                currentNumberOfMember++;
                allmembers.add(uri);
            }
            changeOrder--;

            // Base Page
            if ((buildAll) && (currentNumberOfMember >= BASE_PAGELIMIT)) {
                prevBase = base;
                base = null;
                currentNumberOfMember = 0;
            }
            // ChangeLog Page
            if (currentNumberOfChangeLog >= CHANGELOGS_PAGELIMIT) {
                prevChangeLog = changeLog;
                changeLog = null;
                currentNumberOfChangeLog = 0;
            }
            newChangeEvent(ce);
        }
        // Base resource's CutoffEvent is most recent Change event.
        // Since the order of the returned HistoryData is "new -> old",
        // the first one is the most recent.
        if (buildAll) {
            baseResouces.get("1").setCutoffEvent(mostRecentChangeEventURI);
        }

        for (Entry<String, ChangeLog> changeLogEntry : changeLogs.entrySet()) {
            ChangeLog builtChangeLog = changeLogEntry.getValue();
            Collections.sort(builtChangeLog.getChange(), changeEventComparator);
        }

        log.info("finished building the change log and the base");
    }

}
