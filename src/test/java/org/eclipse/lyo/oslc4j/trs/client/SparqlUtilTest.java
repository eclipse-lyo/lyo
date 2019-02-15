/*
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
package org.eclipse.lyo.oslc4j.trs.client;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
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
import org.eclipse.lyo.oslc4j.trs.client.util.RdfUtil;
import org.eclipse.lyo.oslc4j.trs.client.util.SparqlUtil;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.junit.Assert.*;

public class SparqlUtilTest {

    private static Logger logger = LoggerFactory.getLogger(SparqlUtilTest.class);

    private static Dataset dataset;

    @BeforeClass
    public static void setUpBeforeClass() {
        String dataSetPath = SparqlUtilTest.class.getResource("/test_data_base/test_data_set").getFile();
//        Location directory = Location.create(dataSetPath);
        dataset = TDBFactory.createDataset(dataSetPath);
        clear();
    }

    @AfterClass
    public static void tearDownAfterClass() {
        dataset.close();
    }

    @Before
    public void setUp() {

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
    public void tearDown() {
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
            logger.error("Error marshalling the model", e);
            assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
        executeUpdate(sparqlUpdate);
        dataset.begin(ReadWrite.READ);
        assertTrue(dataset.containsNamedModel(graphName));
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
            logger.error("Error marshalling the model", e);
            assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
        executeUpdate(sparqlUpdate);
        dataset.begin(ReadWrite.READ);
        assertTrue(dataset.containsNamedModel(graphName));
        dataset.commit();
        dataset.end();
        String sparqlUpdateDrop = SparqlUtil.dropGraphQuery(graphName);
        executeUpdate(sparqlUpdateDrop);
        dataset.begin(ReadWrite.READ);
        assertTrue(!dataset.containsNamedModel(graphName));
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
            logger.error("Error marshalling the model", e);
            assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
        executeUpdate(sparqlUpdate);
        dataset.begin(ReadWrite.READ);
        assertTrue(dataset.containsNamedModel(graphName));
        dataset.commit();
        dataset.end();
        String clearGraphQuery = SparqlUtil.removeAllTriplesInGraphQuery(graphName);
        executeUpdate(clearGraphQuery);

        dataset.begin(ReadWrite.READ);
        assertFalse(dataset.containsNamedModel(graphName));
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
            logger.error("Error marshalling the model", e);
            assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
        executeUpdate(sparqlUpdate);

        String statement = "<http://www.w3.org/People/EM/contact#me> <http://www" +
                ".w3.org/2000/10/swap/pim/contact#fullName> \"Eric Miller\" .";

        String addTriplesToGraphQuery = SparqlUtil.addTriplesToGraphQuery(graphName, statement);
        executeUpdate(addTriplesToGraphQuery);
        dataset.begin(ReadWrite.READ);
        try {
            Model namedModel = dataset.getNamedModel(graphName);
            String namedModelNtriples = RdfUtil.modelToNTriple(namedModel);
            assertTrue(namedModelNtriples.contains(statement));
        } catch (IOException e) {
            logger.error("Error marshalling the model", e);
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
            logger.error("Error marshalling the model", e);
            assertFalse(true);
        }
        dataset.commit();
        dataset.end();
        String sparqlUpdate = SparqlUtil.getModificationEventQuery(graphName, defaultModelNTrip);
        executeUpdate(sparqlUpdate);

        dataset.begin(ReadWrite.READ);
        try {
            Model namedModel = dataset.getNamedModel(graphName);
            String namedModelNtriples = RdfUtil.modelToNTriple(namedModel);
            assertTrue(dataset.containsNamedModel(graphName));
            assertTrue(namedModelNtriples.equals(defaultModelNTrip));
        } catch (IOException e) {
            logger.error("Error marshalling the model", e);
        }

        dataset.commit();
        dataset.end();
    }

    private void executeUpdate(String sparqlUpdate) {
        dataset.begin(ReadWrite.WRITE);
        UpdateRequest updateReq = UpdateFactory.create(sparqlUpdate);
        try {
            UpdateAction.execute(updateReq, dataset);
        } catch (Exception e) {
            logger.error("Error updating the triplestore", e);
        }
        dataset.commit();
        dataset.end();

    }

    private static void clear() {
        dataset.begin(ReadWrite.WRITE);

        String sparqlUpdate = "DROP ALL";
        UpdateRequest updateReq = UpdateFactory.create(sparqlUpdate);
        try {
            UpdateAction.execute(updateReq, dataset);
        } catch (Exception e) {
            logger.error("Error clearing the triplestore", e);
        }
        dataset.commit();
        dataset.end();
    }
}
