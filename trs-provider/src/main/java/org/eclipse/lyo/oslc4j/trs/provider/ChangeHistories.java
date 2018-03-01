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
package org.eclipse.lyo.oslc4j.trs.provider;

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
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import javax.servlet.http.HttpServletRequest;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TRSConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is the backbone of the Tracked resource set service class. This
 * class is extended by oslc adapters wishing to implement an OSLC TRS
 * interface. The implementing classes need to implement one method returning
 * the change history of the resources living in the tool exposed by the OSLC
 * adapter
 *
 * @version $version-stub$
 * @since 2.3.0
 */
public abstract class ChangeHistories {

    private final static Logger log = LoggerFactory.getLogger(ChangeHistories.class);

    /**
     * Comparators for change events and history data
     */
    public static Comparator<HistoryData> histDataComparator = new Comparator<HistoryData>() {
        @Override
        public int compare(HistoryData object1, HistoryData object2) {
            return object2.getTimestamp().compareTo(object1.getTimestamp()); // reverse
        }
    };
    public static Comparator<ChangeEvent> changeEventComparator = new Comparator<ChangeEvent>() {
        @Override
        public int compare(ChangeEvent object1, ChangeEvent object2) {
            return object2.getOrder() - object1.getOrder(); //
        }
    };
    private static String mutex = "";//$NON-NLS-1$
    /**
     * Page Limit for Base Resources
     */
    private final int BASE_PAGELIMIT = 40;
    /**
     * Page Limit for ChangeLogs
     */
    private final int CHANGELOGS_PAGELIMIT = 40;
    /**
     * Mutex used for managing concurrent accesses to TRS information
     */
    protected ArtificialTRSMaker artificialTRSMaker = new ArtificialTRSMaker();
    /**
     * Saved History data
     */

    protected List<HistoryData> allHistories = new ArrayList<HistoryData>();
    /**
     * interval for updating the base resources
     */
    protected long UPDATEINTERVAL;
    /**
     * A flag which should be turned on to true in case the inheriting class
     * will be exposing artificial TRS data which using the utility method of
     * the ArtificialTRSMaker class.
     */
    protected boolean artificialTRS = true;
    /**
     * If this flag is turned on, artificial history data will be periodically
     * added for the existing base resources
     */
    protected boolean periodicArtificialTRS = false;
    /**
     * This flag should be set to true by the implementing class in case it
     * generates TRS data for its TRS interface using the artifical TRS maker
     * class functionality.
     */
    protected boolean hasBeenTweaked = false;
    /**
     * url prefix for the trs url e.g http://host/services/ will imply the trs
     * uri shall be of the type http://host/services/trs
     */
    protected String serviceBase;
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

///*    *//**//**
//     * Periodically create artifical TRS data for the base resources if the
//     * flags are both turned on
//     *//**//* {
//        if (artificialTRS && periodicArtificialTRS) {
//
//            final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
//
//            scheduler.scheduleAtFixedRate(new Runnable() {
//
//                @Override
//                public void run() {
//                    if (hasBeenTweaked) {
//                        for (Base base : baseResouces.values()) {
//                            for (URI baseMember : base.getMembers()) {
//                                allHistories.addAll(artificialTRSMaker.getPostTweakedHistoryDataForElement(baseMember));
//                            }
//                        }
//                    }
//
//                }
//            }, 10, 5, TimeUnit.MINUTES);
//        }
//    }*/

    public ChangeHistories(long UPDATEINTERVAL, String serviceBase) {
        this.serviceBase = serviceBase;
        this.UPDATEINTERVAL = UPDATEINTERVAL;
    }

    public ChangeHistories() {
    }

    /**
     * add a list of changes to the existing history data list.
     *
     * @param changeType  the type of change to be added: modification, creation or
     *                    deletion
     * @param changedUris the uris of the changed objects
     */
    public void updateHistories(String changeType, List<URI> changedUris) {
        List<HistoryData> hd = new ArrayList<HistoryData>();
        for (URI changedUri : changedUris) {
            HistoryData histData = HistoryData.getInstance(new Date(), changedUri, changeType);
            hd.add(histData);
        }

        updateHistories(hd);
    }

    public void updateHistories(List<HistoryData> hd) {
        allHistories.addAll(hd);
        Collections.sort(allHistories, histDataComparator);
    }

    /**
     * Implemented by inheriting classes. returns the changes through time to
     * the resources exposes by the adapter starting from the given point in
     * time
     *
     * @param dateAfter the date starting from which the change data is returned
     */
    public abstract HistoryData[] getHistory(HttpServletRequest httpServletRequest, Date dateAfter);

    // protected abstract void init();

    /**
     * Order the history data returned by the getHistory method to use it in the
     * rest of the class
     */
    public HistoryData[] getOrderedHistory(HttpServletRequest httpServletRequest, Date dateAfter) {
        HistoryData[] unsortedHd = getHistory(httpServletRequest, dateAfter);
        Arrays.sort(unsortedHd, histDataComparator);
        return unsortedHd;
    }

    /**
     * A synchronized call to the buildBaseResourcesAndChangeLogsInternal to
     * manage concurrent calls to the TRS service
     */
    public void buildBaseResourcesAndChangeLogs(HttpServletRequest httpServletRequest) throws URISyntaxException {
        synchronized (mutex) {
            buildBaseResourcesAndChangeLogsInternal(httpServletRequest);
        }
    }

    /**
     * Return pagenum's Base resource
     *
     * @param pagenum            the required page of the base
     * @param httpServletRequest the http request for the base page
     *
     * @return the required page of the base
     *
     * @throws URISyntaxException propagated from called method throwing same exception
     */
    public Base getBaseResource(String pagenum, HttpServletRequest httpServletRequest) throws URISyntaxException {
        synchronized (mutex) {
            buildBaseResourcesAndChangeLogsInternal(httpServletRequest);
            return baseResouces != null ? baseResouces.get(pagenum) : null;
        }
    }

    /**
     * Return pagenum's ChangeLog
     *
     * @param pagenum            the required page of the change log
     * @param httpServletRequest the http request for the change log page
     *
     * @return the required page of the change log
     *
     * @throws URISyntaxException propagated from called method throwing same exception
     */
    public ChangeLog getChangeLog(String pagenum, HttpServletRequest httpServletRequest) throws URISyntaxException {
        synchronized (mutex) {
            buildBaseResourcesAndChangeLogsInternal(httpServletRequest);
            // changeLogs might be null
            return changeLogs != null ? changeLogs.get(pagenum) : null;
        }
    }

    public String getServiceBase() {
        return serviceBase;
    }

    public void setServiceBase(String serviceBase) {
        this.serviceBase = serviceBase;
    }

    public long getUPDATEINTERVAL() {
        return UPDATEINTERVAL;
    }

    public void setUPDATEINTERVAL(long uPDATEINTERVAL) {
        UPDATEINTERVAL = uPDATEINTERVAL;
    }

    /**
     * Create the page attribute of the base object
     *
     * @param base        the base object for which this is the page
     * @param basePagenum number of the page
     *
     * @return the page of the base
     */
    private Page createNewBasePage(Base base, int basePagenum) throws URISyntaxException {
        Page ldp = new Page();
        ldp.setAbout(URI.create(serviceBase + "/trs/" + TRSConstants.TRS_TERM_BASE //$NON-NLS-1$
                + String.valueOf(basePagenum)));// );
        ldp.setNextPage(new URI(TRSConstants.RDF_NIL));
        ldp.setPageOf(base);
        return ldp;
    }

    /**
     * Create a new page object with the page basepagenum
     *
     * @param basePagenum the number of this base page
     *
     * @return the created base page object
     */
    private Base createNewBase(int basePagenum) throws URISyntaxException {
        Base base = new Base();
        base.setAbout(URI.create(serviceBase + "/trs/" + TRSConstants.TRS_TERM_BASE));//$NON-NLS-1$

        base.setNextPage(createNewBasePage(base, basePagenum));
        return base;
    }

    /**
     * Main method called to create the change log and the base objects when a
     * call to the TRS service is made. Depending on the refresh rate defined
     * for the trs service, the base objects might be completely rebuilt or only
     * the change logs will be rebuilt
     *
     * @param httpServletRequest the request made to the TRS service
     */
    private void buildBaseResourcesAndChangeLogsInternal(HttpServletRequest httpServletRequest)
            throws URISyntaxException {
        log.info("started building the change log and the base");

        Date nowDate = new Date();
        if ((lastBaseResourceUpdatedDate != null) && (UPDATEINTERVAL != -1) && (nowDate.getTime() -
                lastBaseResourceUpdatedDate
                .getTime() > UPDATEINTERVAL)) {
            mostRecentChangeLogDate = null; // enforce to build all
        }
        boolean buildAll = ((mostRecentChangeLogDate == null) || (baseResouces == null));
        HistoryData[] updatedHistories = null;
        if (!buildAll) {
            // Get only updated Histories
            updatedHistories = getOrderedHistory(httpServletRequest, mostRecentChangeLogDate);
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
            baseResouces = new HashMap<String, Base>();
            baseResouces.put(String.valueOf(basePagenum), base);
        }

        // Try to get histories now.
        HistoryData[] histories = null;
        if (buildAll) {
            histories = getOrderedHistory(httpServletRequest, null);
            if (histories.length == 0) {
                return;
            }
        } else {
            int newSize = prevHistories != null ? prevHistories.length : 0;
            newSize += updatedHistories != null ? updatedHistories.length : 0;
            if (newSize == 0) {
                return;
            }
            histories = new HistoryData[newSize];
            int start = 0;
            if ((updatedHistories != null) && (updatedHistories.length > 0)) {
                System.arraycopy(updatedHistories, 0, histories, start, updatedHistories.length);
                start = updatedHistories.length;
            }
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
        List<URI> allmembers = buildAll ? new ArrayList<URI>(histories.length) : null;
        List<URI> deletedMembers = new ArrayList<URI>();
        for (HistoryData historyData : histories) {
            URI uri = historyData.getUri();
            String changedUriTemplate = "urn:urn-3:" + //$NON-NLS-1$
                    "cm1.example.com" + //$NON-NLS-1$
                    ":" + //$NON-NLS-1$
                    TRSUtil.XSD_DATETIME_FORMAT.format(historyData.getTimestamp()) + ":" + //$NON-NLS-1$
                    changeOrder;
            URI changedUri = URI.create(changedUriTemplate);
            String histDataType = historyData.getType();

            ChangeEvent ce = null;

            if (histDataType == HistoryData.CREATED) {
                ce = new Creation(changedUri, uri, changeOrder);
            } else if (histDataType == HistoryData.MODIFIED) {
                ce = new Modification(changedUri, uri, changeOrder);
            } else {
                ce = new Deletion(changedUri, uri, changeOrder);
                deletedMembers.add(uri);
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
                    prevPage = URI.create(serviceBase + "/trs/" + TRSConstants.TRS_TERM_CHANGE_LOG);//$NON-NLS-1$
                } else {
                    prevPage = URI.create(serviceBase + "/trs/" //$NON-NLS-1$
                            + TRSConstants.TRS_TERM_CHANGE_LOG + "/" + String.valueOf(
                            changeLogPageNum + 1));//$NON-NLS-1$
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
                    changeLogs = new HashMap<String, ChangeLog>();
                }
                changeLogs.put(String.valueOf(changeLogPageNum), changeLog);
            }

            changeLog.getChange().add(ce);
            currentNumberOfChangeLog++;

            if (buildAll && !allmembers.contains(uri) && !deletedMembers.contains(uri)) {
                // Base Page
                if (base == null) {
                    if (prevBase != null) {
                        URI nextPage = URI.create(serviceBase + "/trs/" //$NON-NLS-1$
                                + TRSConstants.TRS_TERM_BASE + "/" + String.valueOf(basePagenum + 1));//$NON-NLS-1$
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
        }
        // Base resource's CutoffEvent is most recent Change event.
        // Since the order of the returned HistoryData is "new -> old",
        // the first one is the most recent.
        if (buildAll) {
            ((Base) baseResouces.get("1")).setCutoffEvent(mostRecentChangeEventURI);//$NON-NLS-1$
        }

        for (Entry<String, ChangeLog> changeLogEntry : changeLogs.entrySet()) {
            ChangeLog builtChangeLog = changeLogEntry.getValue();
            Collections.sort(builtChangeLog.getChange(), changeEventComparator);
        }

        log.info("finished building the change log and the base");
    }

}
