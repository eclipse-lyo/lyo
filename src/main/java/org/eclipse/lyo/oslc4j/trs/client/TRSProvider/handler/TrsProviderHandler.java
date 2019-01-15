/*-
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
package org.eclipse.lyo.oslc4j.trs.client.TRSProvider.handler;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.datatype.DatatypeConfigurationException;
import net.oauth.OAuthException;
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
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import org.eclipse.lyo.oslc4j.trs.client.RepresentationRetrievalException;
import org.eclipse.lyo.oslc4j.trs.client.ServerRollBackException;
import org.eclipse.lyo.oslc4j.trs.client.concurrent.TRSTaskHandler;
import org.eclipse.lyo.oslc4j.trs.client.httpclient.TRSHttpClient;
import org.eclipse.lyo.oslc4j.trs.client.sparql.SparqlUtil;
import org.eclipse.lyo.oslc4j.trs.client.util.ChangeEventComparator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base class for every TRS provider. Handles all periodic operations for a TRS
 * provider. This is the first version which was developed of a TRS provider
 * which does not support any multi threading of A TRS provider's opreations The
 * TRSProviderMultiThreaded class extends this class with support for
 * multithreading.
 *
 *
 * @author Omar
 *
 */
public class TrsProviderHandler extends TRSTaskHandler {
    final static Logger logger = LoggerFactory.getLogger(TrsProviderHandler.class);

    /**
     * The URI of the last processed change event
     */
    protected URI lastProcessedChangeEventUri;

    /**
     * The entry point URI for the tracked resource set of this provider
     */
    protected String trsUriBase;

    public String getSparqlUpdateService() {
        return sparqlUpdateService;
    }

    public void setSparqlUpdateService(String sparqlUpdateService) {
        this.sparqlUpdateService = sparqlUpdateService;
    }

    public String getTrsUriBase() {
        return trsUriBase;
    }

    public void setTrsUriBase(String trsUriBase) {
        this.trsUriBase = trsUriBase;
    }

    public String getUserName() {
        return baseAuth_userName;
    }

    public void setUserName(String userName) {
        this.baseAuth_userName = userName;
    }

    public String getPassword() {
        return baseAuth_pwd;
    }

    public void setPassword(String pwd) {
        this.baseAuth_pwd = pwd;
    }

    public TrsProviderHandler(String trsUriBase, String sparqlQueryService, String sparqlUpdateService,
            TRSHttpClient trsHttpClient, String userName, String pwd, String sparql_user, String sparql_pwd) {
        super(trsHttpClient, sparqlUpdateService, sparqlQueryService, sparql_user, sparql_pwd, userName, pwd);
        this.trsUriBase = trsUriBase;
        threadName = "TRS Provider: " + this.trsUriBase + " thread";
    }

    public TrsProviderHandler(String trsUriBase, String sparqlQueryService, String sparqlUpdateService,
            TRSHttpClient trsHttpClient, String userName, String pwd) {
        super(trsHttpClient, sparqlQueryService, sparqlUpdateService, userName, pwd);
        this.trsUriBase = trsUriBase;
        threadName = "TRS Provider: " + this.trsUriBase + " thread";
    }

    /**
     * Return a list of base objects corresponding to the pages of the base
     * after requesting them from the base url. The base url is retrieved from
     * the trs object passes as a parameter.
     *
     * @param updatedTrs
     *            the trs object retrieved after retrieving it using the trs uri
     * @return the pages of the base of this trs provider
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    public List<Base> updateBases(TrackedResourceSet updatedTrs) throws IOException, OAuthException, URISyntaxException,
    IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
    SecurityException, NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException {
        List<Base> bases = new ArrayList<Base>();
        URI firstBasePageUri = updatedTrs.getBase();
        Base currentBase = fetchRemoteBase(firstBasePageUri.toString());
        Page nextPage = currentBase.getNextPage();
        bases.add(currentBase);
        while (nextPage != null) {
            URI currentPageUri = nextPage.getNextPage();
            if (isNilUri(currentPageUri)) {
                break;
            }
            currentBase = fetchRemoteBase(currentPageUri.toString());
            bases.add(currentBase);
            nextPage = currentBase.getNextPage();
        }
        return bases;
    }

    private boolean isNilUri(URI currentPageUri) {
        return currentPageUri == null || currentPageUri.toString().equals(RDF.nil.getURI());
    }

    /**
     * Return a list of change Lo objects corresponding to the pages of the
     * change log after requesting them from the change log url. The pages of
     * the change log will be requested using the change log segmentation until
     * the last change event which was processed is found. The change log url is
     * retrieved from the trs object passes as a parameter.
     *
     * @param updatedTrs
     *            the trs object retrieved after retrieving it using the trs uri
     * @return the pages of the change log of this trs provider
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws ServerRollBackException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    public List<ChangeLog> fetchUpdatedChangeLogs(TrackedResourceSet updatedTrs)
            throws IOException, OAuthException, URISyntaxException, ServerRollBackException, IllegalAccessException,
            IllegalArgumentException, InstantiationException, InvocationTargetException, SecurityException,
            NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException {

        ChangeLog firstChangeLog = updatedTrs.getChangeLog();
        List<ChangeLog> changeLogs = new ArrayList<ChangeLog>();
        boolean foundSyncEvent = false;

        foundSyncEvent = fetchRemoteChangeLogs(firstChangeLog, changeLogs);
        if (!foundSyncEvent) {
            lastProcessedChangeEventUri = null;
            throw new ServerRollBackException("The sync event can not be found. The sever provinding the trs at: "
                    + trsUriBase + " seems to have been rollecd back to a previous state");
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
     * @param currentChangeLog
     *            the first change log from which the next page will be
     *            retrieved to retrieve the other pages of the change log
     * @param changeLogs
     *            the list of change logs which will be filled with the pages of
     *            the change log
     * @return true if the last processed change event is found, false otherwise
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    public boolean fetchRemoteChangeLogs(ChangeLog currentChangeLog, List<ChangeLog> changeLogs)
            throws IOException, OAuthException, URISyntaxException, IllegalAccessException, IllegalArgumentException,
            InstantiationException, InvocationTargetException, SecurityException, NoSuchMethodException,
            DatatypeConfigurationException, OslcCoreApplicationException {
        boolean foundChangeEvent = false;
        URI previousChangeLog = null;
        do {
            if (currentChangeLog != null) {
                changeLogs.add(currentChangeLog);
                if (changeLogContainsEvent(lastProcessedChangeEventUri, currentChangeLog)) {
                    foundChangeEvent = true;
                    break;
                }
                previousChangeLog = currentChangeLog.getPrevious();
                currentChangeLog = fetchRemoteChangeLog(previousChangeLog.toString());
            } else
                break;
        } while (previousChangeLog != null && !RDF.nil.getURI().equals(previousChangeLog.toString()));
        return foundChangeEvent;
    }

    /**
     * use osl4j functionality to retrieve a TRS object from the rdf model of
     * the TRS returned by the server
     *
     * @param rdFModel
     *            the rdf model
     * @return the TRS pojo extracted from the TRS rdf model
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    protected TrackedResourceSet extractTrsFromRdfModel(Model rdFModel)
            throws OAuthException, URISyntaxException, IllegalAccessException, IllegalArgumentException,
            InstantiationException, InvocationTargetException, SecurityException, NoSuchMethodException,
            DatatypeConfigurationException, OslcCoreApplicationException {
        logger.debug("started extracting tracked resource set from rdf model");
        Object[] trackedResourceSets = null;

        trackedResourceSets = JenaModelHelper.fromJenaModel(rdFModel, TrackedResourceSet.class);

        TrackedResourceSet trs = null;

        if (isNotEmptySingletonArray(trackedResourceSets)
                && trackedResourceSets[0] instanceof TrackedResourceSet) {
            trs = (TrackedResourceSet) trackedResourceSets[0];
        }
        ChangeLog trsChangeLog = extractChangeLogFromRdfModel(rdFModel);
        trs.setChangeLog(trsChangeLog);
        logger.debug("finished extracting tracked resource set from rdf model");
        return trs;
    }

    /**
     * extract the change log projo from the rdf model of the change log
     * returned by the server
     *
     * @param rdFModel
     *            of thr change log
     * @return change log pojo
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    protected ChangeLog extractChangeLogFromRdfModel(Model rdFModel) throws URISyntaxException,
    IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
    SecurityException, NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException {
        logger.debug("started extracting change log from rdf model");
        Object[] changeLogs = null;

        Object[] modifications = null;
        Object[] deletions = null;
        Object[] creations = null;

        changeLogs = JenaModelHelper.fromJenaModel(rdFModel, ChangeLog.class);

        creations = JenaModelHelper.fromJenaModel(rdFModel, Creation.class);
        modifications = JenaModelHelper.fromJenaModel(rdFModel, Modification.class);
        deletions = JenaModelHelper.fromJenaModel(rdFModel, Deletion.class);

        ChangeLog changeLog = null;

        if (isNotEmptySingletonArray(changeLogs)
                && changeLogs[0] instanceof ChangeLog) {
            changeLog = (ChangeLog) changeLogs[0];
        }
        changeLog.getChange().clear();

        if (isNotEmpty(modifications))
            changeLog.getChange().addAll(Arrays.asList((Modification[]) modifications));

        if (isNotEmpty(creations))
            changeLog.getChange().addAll(Arrays.asList((Creation[]) creations));

        if (isNotEmpty(deletions))
            changeLog.getChange().addAll(Arrays.asList((Deletion[]) deletions));
        logger.debug("finished extracting change log set from rdf model");
        return changeLog;
    }

    private boolean isNotEmptySingletonArray(Object[] changeLogs) {
        return changeLogs != null && changeLogs.length < 2 && changeLogs.length > 0;
    }

    private boolean isNotEmpty(Object[] deletions) {
        return deletions != null && deletions.length > 0;
    }


    /**
     * extract the base projo from the rdf model of the base returned by the
     * server
     *
     * @param rdFModel
     *            of the base
     * @return base pojo
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    protected Base extractBaseFromRdfModel(Model rdFModel) throws IllegalAccessException, IllegalArgumentException,
    InstantiationException, InvocationTargetException, SecurityException, NoSuchMethodException,
    DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException {
        logger.debug("started extracting base from rdf model");
        Page nextPage = null;
        Base baseObj = null;
        Object[] nextPageArray = JenaModelHelper.fromJenaModel(rdFModel, Page.class);
        Object[] basesArray = JenaModelHelper.fromJenaModel(rdFModel, Base.class);

        if (isNotEmptySingletonArray(basesArray) && basesArray[0] instanceof Base) {
            baseObj = (Base) basesArray[0];
        }

        if (baseObj == null) {
            return null;
        }

        if (isNotEmptySingletonArray(nextPageArray)
                && nextPageArray[0] instanceof Page) {
            nextPage = (Page) nextPageArray[0];
            if (nextPage == null) {
                return null;
            }
            baseObj.setNextPage(nextPage);
            logger.debug("finished extracting base from rdf model");
            return baseObj;
        }
        logger.debug("finished extracting base from rdf model");
        return null;
    }

    /**
     * retieve the trs from the trs provider using the trs uri attribute and
     * return a trs pojo accordingly
     *
     * @return trs pojo
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    protected TrackedResourceSet extractRemoteTrs() throws IOException, OAuthException, URISyntaxException,
    IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
    SecurityException, NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException {
        Model rdfModel = (Model) fetchTRSRemoteResource(trsUriBase, Model.class);
        return extractTrsFromRdfModel(rdfModel);
    }

    /**
     * * retrieve the change log from the trs provider using the changeLogURI
     * argument return a change log pojo accordingly
     *
     * @param changeLogURl
     *            url of the change log
     * @return change log pojo
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    protected ChangeLog fetchRemoteChangeLog(String changeLogURl) throws IOException, OAuthException, URISyntaxException,
    IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
    SecurityException, NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException {
        Model rdfModel = (Model) fetchTRSRemoteResource(changeLogURl, Model.class);
        return extractChangeLogFromRdfModel(rdfModel);
    }

    /**
     * * retrieve the base from the trs provider using the baseURI argument
     * return a base pojo accordingly
     *
     * @param baseUrl
     *            url of the base
     * @return base pojo
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     */
    protected Base fetchRemoteBase(String baseUrl) throws IOException, OAuthException, URISyntaxException,
    IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
    SecurityException, NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException {
        final Model rdFModel = (Model) fetchTRSRemoteResource(baseUrl, Model.class);
        return extractBaseFromRdfModel(rdFModel);
    }


    /**
     * returns true if the change log pojo contains the change event with the
     * given uri and false otherwise
     *
     * @param syncPointUri
     * @param changeLog
     * @return
     */
    public boolean changeLogContainsEvent(URI syncPointUri, ChangeLog changeLog) {
        for (ChangeEvent changeEvent : changeLog.getChange()) {
            if (changeEvent.getAbout().equals(syncPointUri)) {
                return true;
            }
        }
        return false;
    }

    /**
     * The main method for a TRS provider. This method consists on the periodic
     * process of processing the new change events since last time and the
     * processing of the base in case this is the first time the TRS provider
     * thread is ran
     *
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     * @throws ServerRollBackException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InstantiationException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws DatatypeConfigurationException
     * @throws OslcCoreApplicationException
     * @throws RepresentationRetrievalException
     */
    public void pollAndProcessChanges() throws IOException, OAuthException, URISyntaxException, ServerRollBackException,
    IllegalAccessException, IllegalArgumentException, InstantiationException, InvocationTargetException,
    SecurityException, NoSuchMethodException, DatatypeConfigurationException, OslcCoreApplicationException,
    RepresentationRetrievalException {

        logger.info("started dealing with TRS Provider: " + trsUriBase);

        TrackedResourceSet updatedTrs = extractRemoteTrs();
        boolean indexingStage = false;
        List<URI> baseMembers = new ArrayList<URI>();

        /*
         * If it is the indexing phase retrieve the representation of the base
         */
        if (lastProcessedChangeEventUri == null) {
            List<Base> bases = updateBases(updatedTrs);

            for (Base base : bases) {
                baseMembers.addAll(base.getMembers());
            }

            lastProcessedChangeEventUri = bases.get(0).getCutoffEvent();
            indexingStage = true;
        }
        /*
         * Retrieve all change log pages until the page containing the last
         * processed change event
         */

        List<ChangeLog> changeLogs = fetchUpdatedChangeLogs(updatedTrs);
        /*
         * Optimize the list of changes by removing successive update / creation
         * events for the same resource and overwriting update / creation events
         * of a resource with more recent deletion events
         */
        List<ChangeEvent> compressedChanges = optimizedChangesList(changeLogs);
        /*
         * Process the optimized list of change events
         */
        for (ChangeEvent changeEvent : compressedChanges) {
            try {
                processChangeEvent(changeEvent);
                lastProcessedChangeEventUri = changeEvent.getAbout();
            } catch (Exception e) {
                return;
            }
        }
        /*
         * If it is the indexing stage, remove the resources for which the most
         * recent change event has already been processed and then process the
         * remaining members
         */
        if (indexingStage) {

            baseChangeEventsOptimization(compressedChanges, baseMembers);

            for (URI baseMemberUri : baseMembers) {
                String graphName = baseMemberUri.toString();
                Model graphToUpload = (Model) fetchTRSRemoteResource(graphName, Model.class);
                if (graphToUpload != null) {
                    logger.debug("processing base member " + graphName + " creation event ");
                    SparqlUtil.createGraph(graphName, sparqlUpdateService);
                    SparqlUtil.addTriplesToNamedGraph(graphToUpload, graphName, sparqlUpdateService);
                    logger.debug("finished processing base member " + graphName + " creation event ");
                }

            }
        }

        logger.info("finished dealing with TRS Provider: " + trsUriBase);

    }

    /**
     * remove from the URI list the resources for which an event is already
     * present in the change event list. Done to avoid processing base members
     * uselessly
     *
     * @param compressedChangesList
     *            the optimized list of change events
     * @param baseMembers
     *            the members of the base
     */
    public void baseChangeEventsOptimization(List<ChangeEvent> compressedChangesList, List<URI> baseMembers) {
        for (ChangeEvent changeEvent : compressedChangesList) {
            URI changedResource = changeEvent.getChanged();
            if (baseMembers.contains(changedResource)) {
                baseMembers.remove(changedResource);
            }
        }
    }

    /**
     * 1. create an ordered list of change events from the list of change logs
     * given as an argument 2. Cut the list at the last processed change event
     * 3. Optimize the changes list by removing all redundant events for the
     * same resource
     *
     *
     * @param changeLogs
     *            the list of change logs containing the change events to be
     *            processed
     * @return the optimized ordered list of change events
     */
    public List<ChangeEvent> optimizedChangesList(List<ChangeLog> changeLogs) {
        Collections.reverse(changeLogs);

        ChangeLog firstChangeLog = changeLogs.get(0);
        List<ChangeEvent> changeEvents = firstChangeLog.getChange();
        Collections.sort(changeEvents, new ChangeEventComparator());
        firstChangeLog.setChange(changeEvents);

        changeEvents = firstChangeLog.getChange();

        int indexOfSync = -1;
        for (ChangeEvent changeEvent : changeEvents) {
            if (changeEvent.getAbout().equals(lastProcessedChangeEventUri)) {
                indexOfSync = changeEvents.indexOf(changeEvent);
                break;
            }
        }

        changeEvents = changeEvents.subList(indexOfSync + 1, changeEvents.size());

        firstChangeLog.setChange(changeEvents);

        List<ChangeEvent> changesToProcess = new ArrayList<ChangeEvent>();

        for (ChangeLog changeLog : changeLogs) {
            changesToProcess.addAll(changeLog.getChange());
        }

        Collections.sort(changesToProcess, new ChangeEventComparator());
        List<ChangeEvent> compressedChanges = compressChanges(changesToProcess);
        return compressedChanges;
    }

    /**
     * Create the necessary sparql update for processing the change events and
     * send it to the sparql update service
     *
     * @param changeEvent
     *            the change event to be processed
     * @throws IOException
     * @throws OAuthException
     * @throws URISyntaxException
     */
    protected void processChangeEvent(ChangeEvent changeEvent) throws IOException, OAuthException, URISyntaxException {
        URI changed = changeEvent.getChanged();
        logger.info("processing resource " + changed.toString() + " change event ");

        if (changeEvent instanceof Deletion) {
            SparqlUtil.processChangeEvent(changeEvent, null, sparqlUpdateService);
        } else {

            Model updatedResRepresentation = (Model) fetchTRSRemoteResource(changed.toString(), Model.class);
            if (updatedResRepresentation != null) {
                SparqlUtil.processChangeEvent(changeEvent, updatedResRepresentation, sparqlUpdateService);
            }
        }

        logger.info("finished processing resource " + changed.toString() + " change event ");
    }

    /**
     * takes an ordered list of change events to be processed and compressed the
     * list by removing multiple change events for the same resource and keeping
     * only the latest event for that resource
     *
     * @param changesToProcess
     * @return an ordered optimized list of change events
     */
    public List<ChangeEvent> compressChanges(List<ChangeEvent> changesToProcess) {
        Map<URI, ChangeEvent> resToChangeEventMap = new HashMap<URI, ChangeEvent>();

        for (ChangeEvent changeToProcess : changesToProcess) {
            resToChangeEventMap.put(changeToProcess.getChanged(), changeToProcess);
        }

        List<ChangeEvent> reducedChangesList = null;
        if (!resToChangeEventMap.isEmpty()) {
            reducedChangesList = new ArrayList<ChangeEvent>(resToChangeEventMap.values());
            Collections.sort(reducedChangesList, new ChangeEventComparator());

        } else
            reducedChangesList = new ArrayList<ChangeEvent>();
        return reducedChangesList;
    }

    /**
     * Implementation of the method inherited from the TRSTaskHandler class. a
     * call to the periodic processing of the change events is done. If an
     * exception is thrown it's logged and the uri of the last processed change
     * event is set to null. This means that during the next period the
     * processing of the base will be done all over again
     */
    @Override
    protected void processTRSTask() {
        try {
            super.processTRSTask();
            pollAndProcessChanges();
        } catch (Exception e) {
            logger.error("Uncaught handler error", e);
            lastProcessedChangeEventUri = null;
        }
    }

}
