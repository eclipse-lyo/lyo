/*
 * Copyright (c) 2016-2018   KTH Royal Institute of Technology.
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
package org.eclipse.lyo.oslc4j.trs.client;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.servlet.ServletException;
import javax.ws.rs.WebApplicationException;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.oslc4j.trs.client.util.SparqlUtil;
import org.eclipse.lyo.oslc4j.trs.client.util.RdfUtil;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.RepositoryConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.j2bugzilla.base.Bug;
import com.j2bugzilla.base.BugFactory;
import com.j2bugzilla.base.BugzillaConnector;
import com.j2bugzilla.base.BugzillaException;
import com.j2bugzilla.base.ConnectionException;
import com.j2bugzilla.rpc.LogIn;
import com.j2bugzilla.rpc.ReportBug;

/**
 * contains utility classes to create bugzilla bugs, and to link data in the
 * triplestore together with some randomization.
 *
 * @author Omar
 *
 */
public class ModelCreationUtil {

    private Logger logger = LoggerFactory.getLogger(ModelCreationUtil.class);

    private static String fusekiSparqlUpdateService = "https://vservices.offis.de/rtp/fuseki/v1.0/ldr/update";
    private static String fusekiSparqlQueryService = "https://vservices.offis.de/rtp/fuseki/v1.0/ldr/query";
    private String sesameSparqlUpdateService = "https://vservices.offis.de/rtp/openrdf-sesame/v1.0/repositories/ldr/statements";
    String sesameSparqlQueryService = "https://vservices.offis.de/rtp/openrdf-sesame/v1.0/repositories/ldr";

    private static String lineSep = " \n";

    private static final String[] products = { "TestProduct" };

    private static final String[] components = { "TestComponent" };

    private static final String[] os = { "windows", "Mac OS", "Linux", "All", "Other" };

    private static final String[] versions = { "unspecified" };

    private static final String[] platforms = { "PC", "Macintosh", "All", "Other" };

    private static final String[] summaries = { "this is a change request ", "this is a bug", "this is a task",
    "this is a reminder" };

    private static final String[] descriptions = { "this is a the description of the bug ",
    "this is the description of the task" };

    private static String oslc_cm_prefix = "http://open-services.net/ns/cm#";
    private static String oslc_am_prefix = "http://open-services.net/ns/am#";
    private static String oslc_rm_prefix = "http://open-services.net/ns/rm#";

    private static String oslc_cm_changes = oslc_cm_prefix + "changes";
    private static String oslc_cm_changed_by = oslc_cm_prefix + "changed_by";
    public static String oslc_rm_refined_by = oslc_rm_prefix + "refined_by";
    private static String oslc_rm_refines = oslc_rm_prefix + "refines";

    private static String oslc_am_implements = oslc_am_prefix + "implements";

    public void randomChangeRequestsCreationTest() throws ServletException, BugzillaException {

        BugzillaConnector bc = login("http://10.238.2.145", "@tc.informatik.uni-oldenburg.de", "m7fMXrBgwspd");
        ThreadLocalRandom rand = ThreadLocalRandom.current();
        for (int i = 0; i != 500; i++) {
            int randomInt = rand.nextInt(0, 500);
            String randomString = String.valueOf(randomInt);
            String productId = products[rand.nextInt(products.length)];
            String summary = summaries[rand.nextInt(summaries.length)] + randomString;
            String component = components[rand.nextInt(components.length)];
            String version = versions[rand.nextInt(versions.length)];
            String operatingSystem = os[rand.nextInt(os.length)];
            String platform = platforms[rand.nextInt(platforms.length)];
            String description = descriptions[rand.nextInt(descriptions.length)] + randomString;
            createBug(bc, summary, component, version, operatingSystem, platform, description, productId);

        }

    }

    private static BugzillaConnector login(String bugzillaUri, String user, String pwd)
            throws ServletException, BugzillaException {
        BugzillaConnector bc = new BugzillaConnector();

        try {
            bc.connectTo(bugzillaUri + "/xmlrpc.cgi", user, pwd);
        } catch (ConnectionException e) {
            throw new ServletException(e);
        }

        LogIn login = new LogIn(user, pwd);

        bc.executeMethod(login);

        return bc;
    }

    private static String createBug(BugzillaConnector bc, String summary, String component,
            String version, String operatingSystem, String platform, String description,
            final String productIdString) {
        String newBugId;
        try {

            // final int productId = Integer.parseInt(productIdString);
            // GetProduct getProducts = new GetProduct(productId);
            // bc.executeMethod(getProducts);
            //
            // final Product product = getProducts.getProduct();

            BugFactory factory = new BugFactory().newBug().setProduct(productIdString);
            if (summary != null) {
                factory.setSummary(summary);
            }
            if (version != null) {
                factory.setVersion(version);
            }
            if (component != null) {
                factory.setComponent(component);
            }
            if (platform != null) {
                factory.setPlatform(platform);
            } else
                factory.setPlatform("Other");

            if (operatingSystem != null) {
                factory.setOperatingSystem(operatingSystem);
            } else
                factory.setOperatingSystem("Other");

            if (description != null) {
                factory.setDescription(description);
            }

            Bug bug = factory.createBug();
            ReportBug reportBug = new ReportBug(bug);
            bc.executeMethod(reportBug);
            newBugId = Integer.toString(reportBug.getID());

        } catch (Exception e) {
            e.printStackTrace();
            throw new WebApplicationException(e);
        }
        return newBugId;
    }

    /**
     * Query for retrieving all the elements that are part of the matlab world
     * and bugzilla change requests
     */

    public static void linkBugzToSimAndReq() {
        RepositoryConnection conn = SparqlUtil.getRepoConnection(fusekiSparqlQueryService, fusekiSparqlUpdateService,
                                                                 "", "");
        conn.begin();
        try {
            StringBuilder matlabQb = new StringBuilder();
            matlabQb.append("SELECT DISTINCT ?s  \n" + "WHERE \n" + "{" + "\n GRAPH ?g \n" + "{"
                    + "\n ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?o .FILTER contains(str(?o), \"http://mathworks.com/simulink/rdf\") . "
                    + "\n }" + "}");

            StringBuilder bugzQb = new StringBuilder();
            bugzQb.append("SELECT DISTINCT ?s \n" + "WHERE \n" + "{" + "\n GRAPH ?g \n" + "{ \n" + "?s ?p ?o . "
                    + "\n } FILTER contains(str(?s), \"changeRequests" + "/" + "\") . " + "}");

            StringBuilder reqifQb = new StringBuilder();
            reqifQb.append("SELECT DISTINCT ?s \n" + "WHERE \n" + "{" + "\n GRAPH ?g \n" + "{ \n" + "?s ?p ?o . "
                    + "\n } FILTER contains(str(?s), \"rm-services" + "/" + "\") . " + "}");

            TupleQueryResult bugzQueryResult = SparqlUtil.evalQuery(conn, bugzQb.toString());

            TupleQueryResult matlabQueryresult = SparqlUtil.evalQuery(conn, matlabQb.toString());

            TupleQueryResult reqIfQueryResult = SparqlUtil.evalQuery(conn, reqifQb.toString());

            String updateQuery = new String();
            List<String> matlabResources = new ArrayList<String>();
            List<String> bugzillaResources = new ArrayList<String>();
            List<String> reqIfResources = new ArrayList<String>();

            while (matlabQueryresult.hasNext()) {
                BindingSet maltabbs = matlabQueryresult.next();
                Value matlabRes = maltabbs.getValue("s");
                String mtlabResStringVal = matlabRes.stringValue();
                matlabResources.add(mtlabResStringVal);
            }

            while (bugzQueryResult.hasNext()) {
                BindingSet bugzBs = bugzQueryResult.next();
                Value bugzRes = bugzBs.getValue("s");
                String bugzResStringVal = bugzRes.stringValue();
                bugzillaResources.add(bugzResStringVal);
            }

            while (reqIfQueryResult.hasNext()) {
                BindingSet reqIfBs = reqIfQueryResult.next();
                Value reqIfRes = reqIfBs.getValue("s");
                String reqIfResStringVal = reqIfRes.stringValue();
                reqIfResources.add(reqIfResStringVal);
            }
            Random random = ThreadLocalRandom.current();

            Collections.shuffle(matlabResources, random);
            Collections.shuffle(bugzillaResources, random);
            Collections.shuffle(reqIfResources, random);

            for (String bugzResStringVal : bugzillaResources) {
                int i = 0;
                List<String> simRestoBeRemoved = new ArrayList<String>();
                for (String mtlabResStringVal : matlabResources) {
                    i++;
                    boolean links = random.nextBoolean();
                    if (links) {
                        String changesTriple = SparqlUtil.linkTriple(bugzResStringVal, mtlabResStringVal,
                                oslc_cm_changes);
                        String changedTriple = SparqlUtil.linkTriple(mtlabResStringVal, bugzResStringVal,
                                oslc_cm_changed_by);

                        String changesQuery = SparqlUtil.addTriplesToGraphQuery(bugzResStringVal, changesTriple);
                        String changedQuery = SparqlUtil.addTriplesToGraphQuery(mtlabResStringVal, changedTriple);

                        updateQuery = SparqlUtil.appendSparqldQuery(updateQuery, changesQuery);
                        updateQuery = SparqlUtil.appendSparqldQuery(updateQuery, changedQuery);

                    }
                    simRestoBeRemoved.add(mtlabResStringVal);
                    if (i == 40)
                        break;
                }
                i = 0;
                for (String reqIfResStringVal : reqIfResources) {
                    i++;
                    boolean links = random.nextBoolean();
                    if (links) {
                        String changesTriple = SparqlUtil.linkTriple(bugzResStringVal, reqIfResStringVal,
                                oslc_cm_changes);

                        String changesQuery = SparqlUtil.addTriplesToGraphQuery(bugzResStringVal, changesTriple);

                        updateQuery = SparqlUtil.appendSparqldQuery(updateQuery, changesQuery);


                    }
                    if (i == 40)
                        break;
                }
                Collections.shuffle(reqIfResources, random);
                Collections.shuffle(matlabResources, random);
            }

            SparqlUtil.processQuery_sesame(updateQuery, conn);
        } finally

        {
            conn.close();
        }
    }

    /**
     * Query for retrieving all the elements that are part of the matlab world
     * and bugzilla change requests
     */
    private static void linkReqToReq() {
        RepositoryConnection conn = SparqlUtil.getRepoConnection(fusekiSparqlQueryService, fusekiSparqlUpdateService,
                "", "");
        conn.begin();
        try {
            StringBuilder reqifQb = new StringBuilder();

            reqifQb.append("SELECT DISTINCT ?s \n" + "WHERE \n" + "{" + "\n GRAPH ?g \n" + "{ \n" + "?s ?p ?o . "
                    + "\n } FILTER contains(str(?s), \"rm-services" + "/" + "\") . " + "}");

            TupleQueryResult reqIfQueryResult = SparqlUtil.evalQuery(conn, reqifQb.toString());

            String updateQuery = new String();
            List<String> reqIfResources = new ArrayList<String>();

            while (reqIfQueryResult.hasNext()) {
                BindingSet reqIfBs = reqIfQueryResult.next();
                Value reqIfRes = reqIfBs.getValue("s");
                String reqIfResStringVal = reqIfRes.stringValue();
                reqIfResources.add(reqIfResStringVal);
            }

            Random random = ThreadLocalRandom.current();

            Collections.shuffle(reqIfResources, random);
            int i = 0;
            for (Iterator<String> iterator = reqIfResources.iterator(); iterator.hasNext();) {
                i++;
                String reqIfResStringVal = iterator.next();

                int[] ex = { reqIfResources.indexOf(reqIfResStringVal) };
                if (reqIfResources.size() > 1) {
                    int randomRefinedReqIndex = getRandomWithExclusion(random, 0, reqIfResources.size() - 1, ex);

                    String randomRefinedReq = reqIfResources.get(randomRefinedReqIndex);

                    String implementsTriple = SparqlUtil.linkTriple(reqIfResStringVal, randomRefinedReq,
                            oslc_rm_refines);

                    String refinesUpdate = SparqlUtil.addTriplesToGraphQuery(reqIfResStringVal, implementsTriple);

                    updateQuery = SparqlUtil.appendSparqldQuery(updateQuery, refinesUpdate);
                    iterator.remove();
                }

            }

            SparqlUtil.processQuery_sesame(updateQuery, conn);
        } finally

        {
            conn.close();
        }
    }

    /**
     * Query for retrieving all the elements that are part of the matlab world
     * and bugzilla change requests
     */

    public static void linkSimulinkToReq() {
        RepositoryConnection conn = SparqlUtil.getRepoConnection(fusekiSparqlQueryService, fusekiSparqlUpdateService,
                "", "");
        conn.begin();
        try {
            StringBuilder matlabQb = new StringBuilder();
            matlabQb.append("SELECT DISTINCT ?s  \n" + "WHERE \n" + "{" + "\n GRAPH ?g \n" + "{"
                    + "\n ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?o .FILTER contains(str(?o), \"http://mathworks.com/simulink/rdf\") . "
                    + "\n }" + "}");

            StringBuilder reqifQb = new StringBuilder();
            reqifQb.append("SELECT DISTINCT ?s \n" + "WHERE \n" + "{" + "\n GRAPH ?g \n" + "{ \n" + "?s ?p ?o . "
                    + "\n } FILTER contains(str(?s), \"rm-services" + "/" + "\") . " + "}");

            TupleQueryResult matlabQueryresult = SparqlUtil.evalQuery(conn, matlabQb.toString());

            TupleQueryResult reqIfQueryResult = SparqlUtil.evalQuery(conn, reqifQb.toString());

            String updateQuery = new String();
            List<String> matlabResources = new ArrayList<String>();
            List<String> reqIfResources = new ArrayList<String>();

            while (matlabQueryresult.hasNext()) {
                BindingSet maltabbs = matlabQueryresult.next();
                Value matlabRes = maltabbs.getValue("s");
                String mtlabResStringVal = matlabRes.stringValue();
                matlabResources.add(mtlabResStringVal);
            }

            while (reqIfQueryResult.hasNext()) {
                BindingSet reqIfBs = reqIfQueryResult.next();
                Value reqIfRes = reqIfBs.getValue("s");
                String reqIfResStringVal = reqIfRes.stringValue();
                reqIfResources.add(reqIfResStringVal);
            }

            Random random = ThreadLocalRandom.current();

            Collections.shuffle(matlabResources, random);
            Collections.shuffle(reqIfResources, random);

            for (String reqIfResStringVal : reqIfResources) {
                int i = 0;
                for (String mtlabResStringVal : matlabResources) {
                    i++;
                    boolean links = random.nextBoolean();
                    if (links) {
                        String implementsTriple = SparqlUtil.linkTriple(mtlabResStringVal, reqIfResStringVal,
                                oslc_am_implements);
                        String changesQuery = SparqlUtil.addTriplesToGraphQuery(mtlabResStringVal, implementsTriple);
                        updateQuery = SparqlUtil.appendSparqldQuery(updateQuery, changesQuery);
                    }
                    if (i == 40)
                        break;
                }
                Collections.shuffle(matlabResources, random);
            }

            SparqlUtil.processQuery_sesame(updateQuery, conn);
        } finally

        {
            conn.close();
        }
    }

    /**
     * Test the correct functionality of the concantenation of several sparql
     * updates
     */

    public void addTripleToGraphTest() {

        String triple = "<http://one.example/subject1> <http://one.example/predicate1> <http://one.example/object1> .";
        String modificationEventQuery = SparqlUtil
                .getModificationEventQuery("http://waals:8085/OSLC4JBugzilla/services/1/changeRequests/3", triple);
        SparqlUtil.processQuery(modificationEventQuery, fusekiSparqlUpdateService);
    }

    /**
     * Test the correct functionality of the concantenation of several sparql
     * updates
     */

    public void sparqlQuerySesameTest() {
        String triple = "<http://one.example/subject1> <http://one.example/predicate1> <http://one.example/object1> .";
        String modificationEventQuery = SparqlUtil
                .getModificationEventQuery("http://waals:8085/OSLC4JBugzilla/services/1/changeRequests/3", triple);
        SparqlUtil.processQuery(modificationEventQuery, fusekiSparqlUpdateService);
    }

    /**
     * Test the correct functionality of the concantenation of several sparql
     * updates
     */

    public void sparqlUpdateSesameTest() {
        String triple = "<http://one.example/subject1> <http://one.example/predicate1> <http://one.example/object1> .";
        String modificationEventQuery = SparqlUtil.getModificationEventQuery("http://waals:3030/example/exampleGraph2",
                triple);
        SparqlUtil.processQuery(modificationEventQuery, sesameSparqlUpdateService);
    }

    /**
     * Test the correct functionality of the concantenation of several sparql
     * updates
     */

    public void sparqlConcatTest() {
        String triple = "<http://one.example/subject1> <http://one.example/predicate1> <http://one.example/object1> .";
        String modificationEventQuery = SparqlUtil
                .getModificationEventQuery("http://waals:8085/OSLC4JBugzilla/services/1/changeRequests/3", triple);
        SparqlUtil.processQuery(modificationEventQuery, fusekiSparqlUpdateService);
    }

    public void sparqlConstructTestTest() {
        String queryString = "CONSTRUCT { ?x ?y ?z}" + lineSep + "WHERE {" + lineSep
                + "GRAPH <http://waals:8085/OSLC4JBugzilla/services/1/changeRequests/10>  {" + lineSep + "?x ?y ?z"
                + lineSep + "FILTER (" + lineSep
                + "?x = <http://waals:8085/OSLC4JBugzilla/services/1/changeRequests/10>" + lineSep + ")" + lineSep
                + "} " + lineSep + "} ";
        Query query = QueryFactory.create(queryString);
        QueryExecution qe = QueryExecutionFactory.sparqlService(fusekiSparqlQueryService, query);
        final Model resulModel = qe.execConstruct();
        try {
            String modelRdfRepresentation = RdfUtil.modelToRdfXml(resulModel);
            logger.debug(modelRdfRepresentation);
        } catch (IOException e) {
            logger.error("Failure while serializing the model");
        }

    }


    public static void main(String[] args) {
        linkReqToReq();
    }

    private static int getRandomWithExclusion(Random rnd, int start, int end, int... exclude) {
        try {
            int random = start + rnd.nextInt(end - start + 1 - exclude.length);
            for (int ex : exclude) {
                if (random < ex) {
                    break;
                }
                random++;
            }
            return random;
        } catch (Exception e) {
            e.printStackTrace();
            return -1;
        }

    }
}
