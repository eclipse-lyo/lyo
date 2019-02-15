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
package org.eclipse.lyo.oslc4j.trs.client.handlers;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.jena.vocabulary.RDF;
import org.eclipse.lyo.core.trs.Base;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.ChangeLog;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Deletion;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.core.trs.Page;
import org.eclipse.lyo.core.trs.TrackedResourceSet;
import org.eclipse.lyo.oslc4j.trs.client.handlers.TrsProviderHandler;
import org.eclipse.lyo.oslc4j.trs.client.util.TrsBasicAuthOslcClient;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;


public class TrsProviderTest {

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        trsProvider = new TrsProviderHandler("", "", "", new TrsBasicAuthOslcClient(), "", "", null, null);
        trs = new TrackedResourceSet();
        cl_p1 = new ChangeLog();
        cl_p2 = new ChangeLog();
        creation = new Creation();
        deletion = new Deletion();
        modif = new Modification();
        b_p1 = new Base();
        b_p2 = new Base();
    }

    @Before
    public void setUp() throws Exception {
        initChangeEvent(creation);
        initChangeEvent(deletion);
        initChangeEvent(modif);

        trs.setAbout(new URI(changeLogUri()));

        cl_p1.setAbout(new URI(changeLogUri()));
        cl_p2.setAbout(new URI(changeLogUri()));

        initChangeLog(cl_p1, cl_p2.getAbout());
        initChangeLog(cl_p2, null);

        b_p1.setAbout(new URI(baseUri()));
        b_p2.setAbout(new URI(baseUri()));

        initBase(b_p1, b_p2.getAbout());
        initBase(b_p2, null);

        trs.setAbout(new URI(trackedResourceSetUri()));

        trs.setBase(b_p1.getAbout());
        trs.setChangeLog(cl_p1);
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
    }

    @After
    public void tearDown() throws Exception {
        trsProvider = new TrsProviderHandler("", "", "", new TrsBasicAuthOslcClient(), "", "", null, null);
        trs = new TrackedResourceSet();
        cl_p1 = new ChangeLog();
        cl_p2 = new ChangeLog();
        creation = new Creation();
        deletion = new Deletion();
        modif = new Modification();
        b_p1 = new Base();
        b_p2 = new Base();
    }

    @Test
    public final void testChangeLogContainsEvent() throws URISyntaxException {
        ChangeEvent ce = cl_p1.getChange().get(0);
        Assert.assertTrue(trsProvider.changeLogContainsEvent(ce.getAbout(), cl_p1));
        URI uri = new URI("http", "google.com", "/test", null);
        Assert.assertFalse(trsProvider.changeLogContainsEvent(uri, cl_p1));
    }

    @Test
    public final void testBaseChangeEventsOptimization() {
        List<ChangeEvent> changeEventsList = new ArrayList<ChangeEvent>();
        changeEventsList.add(creation);
        changeEventsList.add(modif);

        URI changedCreation = creation.getChanged();
        URI changedModification = modif.getChanged();
        URI changedDeletion = deletion.getChanged();

        b_p1.getMembers().add(changedCreation);
        b_p1.getMembers().add(changedModification);
        b_p1.getMembers().add(changedDeletion);

        trsProvider.baseChangeEventsOptimization(changeEventsList, b_p1.getMembers());

        Assert.assertFalse(b_p1.getMembers().contains(changedCreation));
        Assert.assertFalse(b_p1.getMembers().contains(changedModification));
        Assert.assertTrue(b_p1.getMembers().contains(changedDeletion));
    }

    @Test
    public final void testOptimizedChangesList() throws URISyntaxException {
        String baseMemberUri1 = baseMemberUri();

        String baseMemberUri2 = baseMemberUri();

        Creation memb1_c1 = new Creation();
        Modification memb1_m1 = new Modification();
        Modification memb1_m2 = new Modification();
        Deletion memb1_d1 = new Deletion();

        initChangeEvent(memb1_c1, baseMemberUri1);
        initChangeEvent(memb1_m1, baseMemberUri1);
        initChangeEvent(memb1_m2, baseMemberUri1);
        initChangeEvent(memb1_d1, baseMemberUri1);

        Creation memb2_c1 = new Creation();
        Modification memb2_m1 = new Modification();
        Modification memb2_m2 = new Modification();


        initChangeEvent(memb2_c1, baseMemberUri2);
        initChangeEvent(memb2_m1, baseMemberUri2);
        initChangeEvent(memb2_m2, baseMemberUri2);

        ChangeLog changeLog = new ChangeLog();

        changeLog.getChange().add(memb1_c1);
        changeLog.getChange().add(memb1_m1);
        changeLog.getChange().add(memb1_m2);
        changeLog.getChange().add(memb1_d1);
        changeLog.getChange().add(memb2_c1);
        changeLog.getChange().add(memb2_m1);
        changeLog.getChange().add(memb2_m2);

        List<ChangeLog> changeLogsList = Collections.singletonList(changeLog);

        List<ChangeEvent> changeEventsList = trsProvider.optimizedChangesList(changeLogsList);

        Assert.assertTrue(changeEventsList.size() == 2);

        Assert.assertTrue(changeEventsList.contains(memb1_d1));
        Assert.assertTrue(changeEventsList.contains(memb2_m2));

        Assert.assertFalse(changeEventsList.contains(memb1_c1));
        Assert.assertFalse(changeEventsList.contains(memb1_m1));
        Assert.assertFalse(changeEventsList.contains(memb1_m2));
        Assert.assertFalse(changeEventsList.contains(memb2_c1));
        Assert.assertFalse(changeEventsList.contains(memb2_m1));

    }

    private String changeEventUri() {
        return ceUriPrefix + "/" + changeEventNum++;
    }

    private String baseMemberUri() {
        return ceUriPrefix + "/" + baseMemberNum++;
    }

    private String baseUri() {
        return bUriPrefix + "/" + baseNum++;
    }

    private String changeLogUri() {

        return clUriPrefix + "/" + changeLogNum++;
    }

    private String trackedResourceSetUri() {

        return trsUriPrefix + "/" + trackedResourceSetNum++;
    }

    private void initChangeEvent(ChangeEvent ce) throws URISyntaxException {
        java.net.URI changeEvenUri = new URI(changeEventUri());
        java.net.URI baseMemberUri = new URI(baseMemberUri());

        ce.setAbout(changeEvenUri);
        ce.setChanged(baseMemberUri);
        ce.setOrder(changeEventNum);

    }

    private void initChangeEvent(ChangeEvent ce, String baseMemberUriString) throws URISyntaxException {
        java.net.URI changeEvenUri = new URI(changeEventUri());
        java.net.URI baseMemberUri = new URI(baseMemberUriString);

        ce.setAbout(changeEvenUri);
        ce.setChanged(baseMemberUri);
        ce.setOrder(changeEventNum);

    }

    private void initBase(Base base, URI previous) throws URISyntaxException {

        base.setCutoffEvent(new URI(changeEventUri()));

        List<URI> b_p1_members = new ArrayList<URI>();
        b_p1_members.add(new URI(baseMemberUri()));
        b_p1_members.add(new URI(baseMemberUri()));
        b_p1_members.add(new URI(baseMemberUri()));
        base.setMembers(b_p1_members);
        Page ldp = new Page();
        if (previous != null) {

            ldp.setAbout(URI.create(bUriPrefix + "/" //$NON-NLS-1$
                    + String.valueOf(baseNum)));// );
            ldp.setPageOf(base);
            ldp.setNextPage(previous);
        } else
            ldp.setNextPage(rdfNiluri);
        base.setNextPage(ldp);

    }

    private void initChangeLog(ChangeLog cl, URI previous) throws URISyntaxException {
        List<ChangeEvent> changeEvents_p1 = new ArrayList<ChangeEvent>();

        Modification me_p1 = new Modification();
        Creation ce_p1 = new Creation();
        Deletion de_p1 = new Deletion();

        changeEvents_p1.add(me_p1);
        changeEvents_p1.add(ce_p1);
        changeEvents_p1.add(de_p1);

        for (ChangeEvent ce : changeEvents_p1) {
            initChangeEvent(ce);
        }

        cl.getChange().addAll(changeEvents_p1);

        if (previous != null)
            cl.setPrevious(previous);
        else
            cl.setPrevious(rdfNiluri);
    }

    private static URI rdfNiluri;

    static {
        try {
            rdfNiluri = new URI(RDF.nil.getURI());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
    }

    static String uriPrefix = "https://host";

    static String ceUriPrefix = uriPrefix + "/changeEvents";
    static String bmUriPrefix = uriPrefix + "/baseMembers";
    static String bUriPrefix = uriPrefix + "/bases";

    static String clUriPrefix = uriPrefix + "/changeLogs";
    static String trsUriPrefix = uriPrefix + "/trackedResourceSets";

    static int changeEventNum = 0;
    static int baseMemberNum = 0;
    static int changeLogNum = 0;
    static int trackedResourceSetNum = 0;
    static int baseNum = 0;

    static TrackedResourceSet trs;
    static ChangeLog cl_p1;
    static ChangeLog cl_p2;
    static Creation creation;
    static Deletion deletion;
    static Modification modif;
    static Base b_p1;
    static Base b_p2;
    static TrsProviderHandler trsProvider;

}
