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
package org.eclipse.lyo.oslc4j.trs.client.test;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import junit.framework.Assert;
import org.apache.commons.io.FileUtils;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.tdb.TDBFactory;
import org.apache.jena.tdb.TDBLoader;
import org.apache.jena.tdb.sys.TDBInternal;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateRequest;
import org.apache.log4j.Logger;
import org.eclipse.lyo.oslc4j.trs.client.rdf.RdfUtil;
import org.eclipse.lyo.oslc4j.trs.client.sparql.SparqlUtil;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class SparqlUtilTest {

    static Logger logger = Logger.getLogger(SparqlUtilTest.class);

    static Dataset dataset;

    private static void clear() throws URISyntaxException {
        dataset.begin(ReadWrite.WRITE);

        String sparqlUpdate = "DROP ALL";
        UpdateRequest updateReq = UpdateFactory.create(sparqlUpdate);
        try {
            UpdateAction.execute(updateReq, dataset);
        } catch (Exception e) {
            logger.error(e);
        }
        dataset.commit();
        dataset.end();
    }

    private void executeUpdate(String sparqlUpdate) throws URISyntaxException {
        dataset.begin(ReadWrite.WRITE);
        UpdateRequest updateReq = UpdateFactory.create(sparqlUpdate);
        try {
            UpdateAction.execute(updateReq, dataset);
        } catch (Exception e) {
            logger.error(e);
        }
        dataset.commit();
        dataset.end();

    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        String dataSetPath = SparqlUtilTest.class.getResource("/test_data_base/test_data_set").getFile();
//        Location directory = Location.create(dataSetPath);
        dataset = TDBFactory.createDataset(dataSetPath);
        clear();
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        dataset.close();
    }

    @Before
    public void setUp() throws Exception {

        dataset.begin(ReadWrite.WRITE);
        URL elvisModel = SparqlUtilTest.class.getResource("/test_data_base/elvisimp.rdf");

        try {
            File elvisModelFile = FileUtils.toFile(elvisModel);
            InputStream in = new BufferedInputStream(new FileInputStream(elvisModelFile.getAbsolutePath()));
            TDBLoader.load(TDBInternal.getDatasetGraphTDB(dataset.asDatasetGraph()), in, true);
        } catch (Throwable t) {
            t.printStackTrace();
        }
        dataset.commit();
        dataset.end();
    }

    @After
    public void tearDown() throws Exception {
        clear();
    }

    @Test
    public final void testCreateGraphQuery() {
        String graphName = "http://graph_whateveuuuur.com";
        dataset.begin(ReadWrite.READ);
        Model defaultModel = dataset.getDefaultModel();
        String defaultModelNTrip = null;
        try {
            defaultModelNTrip = RdfUtil.modelToNTriple(defaultModel);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        try {
            String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName,
                    defaultModelNTrip);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.begin(ReadWrite.READ);
        Assert.assertTrue(dataset.containsNamedModel(graphName));
        dataset.commit();
        dataset.end();
    }

    @Test
    public final void testDropGraphQuery() {
        String graphName = "http://graph_whateveuuuur.com";
        dataset.begin(ReadWrite.READ);
        Model defaultModel = dataset.getDefaultModel();
        String defaultModelNTrip = null;
        try {
            defaultModelNTrip = RdfUtil.modelToNTriple(defaultModel);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        try {
            String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.begin(ReadWrite.READ);
        Assert.assertTrue(dataset.containsNamedModel(graphName));
        dataset.commit();
        dataset.end();
        try {
            String sparqlUpdate = SparqlUtil.dropGraphQuery(graphName);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.begin(ReadWrite.READ);
        Assert.assertTrue(!dataset.containsNamedModel(graphName));
        dataset.commit();
        dataset.end();
    }
    //
    @Test
    public final void testRemoveAllTriplesInGraphQuery() {
        String graphName = "http://graph_whateveuuuur.com";
        dataset.begin(ReadWrite.READ);
        Model defaultModel = dataset.getDefaultModel();
        String defaultModelNTrip = null;
        try {
            defaultModelNTrip = RdfUtil.modelToNTriple(defaultModel);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        try {
            String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.begin(ReadWrite.READ);
        Assert.assertTrue(dataset.containsNamedModel(graphName));
        dataset.commit();
        dataset.end();
        try {
            String sparqlUpdate = SparqlUtil.removeAllTriplesInGraphQuery(graphName);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }

        dataset.begin(ReadWrite.READ);
        Assert.assertFalse(dataset.containsNamedModel(graphName));
        dataset.commit();
        dataset.end();
    }

    @Test
    public final void testAddTriplesToGraphQueryStringModel() {
        String graphName = "http://graph_whateveuuuur.com";
        dataset.begin(ReadWrite.READ);
        Model defaultModel = dataset.getDefaultModel();
        String defaultModelNTrip = null;
        try {
            defaultModelNTrip = RdfUtil.modelToNTriple(defaultModel);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        try {
            String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }

        String statement = "<http://www.w3.org/People/EM/contact#me> <http://www.w3.org/2000/10/swap/pim/contact#fullName> \"Eric Miller\" .";

        try {
            String sparqlUpdate = SparqlUtil.addTriplesToGraphQuery(graphName, statement);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.begin(ReadWrite.READ);
        try {
            Model namedModel = dataset.getNamedModel(graphName);
            String namedModelNtriples = RdfUtil.modelToNTriple(namedModel);
            Assert.assertTrue(namedModelNtriples.contains(statement));
        } catch (IOException e) {
            logger.error(e);
        }

        dataset.commit();
        dataset.end();

    }

    @Test
    public final void testGetModificationEventQueryChangeEventModel() {
        String graphName = "http://graph_whateveuuuur.com";
        dataset.begin(ReadWrite.READ);
        Model defaultModel = dataset.getDefaultModel();
        String defaultModelNTrip = null;
        try {
            defaultModelNTrip = RdfUtil.modelToNTriple(defaultModel);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        try {
            String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
            executeUpdate(sparqlUpdate);
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            logger.error(e);
            Assert.assertFalse(true);
        }


        dataset.begin(ReadWrite.READ);
        try {
            Model namedModel = dataset.getNamedModel(graphName);
            String namedModelNtriples = RdfUtil.modelToNTriple(namedModel);
            Assert.assertTrue(dataset.containsNamedModel(graphName));
            Assert.assertTrue(namedModelNtriples.equals(defaultModelNTrip));
        } catch (IOException e) {
            logger.error(e);
        }

        dataset.commit();
        dataset.end();
    }
    //
    // @Test
    // public final void testAddTriplesToGraphQueryStringString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testGetChangeEventQuery() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    //
    // @Test
    // public final void testGetModificationEventQueryChangeEventString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testGetModificationEventQueryStringString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testGetCreationEventQuery() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testGetDeletionEventQuery() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testCreateGraph() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testDropGraph() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testAddTriplesToNamedGraph() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testRemoveAllTriplesInNamedGraph() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testProcessChangeEvent() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testProcessQuery() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testProcessQuery_sesameStringStringStringString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testProcessQuery_sesameStringRepositoryConnection() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testGetRepoConnectionStringStringString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testGetRepoConnectionStringStringStringString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testEvalQueryStringStringStringString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testEvalUpdate() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testEvalQueryRepositoryConnectionString() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testAppendSparqldQuery() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testProcessTripleAdditionQuery() {
    // fail("Not yet implemented"); // TODO
    // }
    //
    // @Test
    // public final void testLinkTriple() {
    // fail("Not yet implemented"); // TODO
    // }

}
