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
package org.eclipse.lyo.oslc4j.trs.server;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.UUID;
import org.eclipse.lyo.core.trs.ChangeEvent;
import org.eclipse.lyo.core.trs.Creation;
import org.eclipse.lyo.core.trs.Modification;
import org.eclipse.lyo.oslc4j.trs.server.HistoryData;
import org.eclipse.lyo.oslc4j.trs.server.TRSUtil;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

@SuppressWarnings("deprecation")
public class TRSUtilTest {
    static HistoryData hd;
    static ChangeEvent ce;
    static String uriPrefix      = "https://host";
    static String ceUriPrefix    = uriPrefix + "/changeEvents";
    static int    changeEventNum = 0;
    static int    baseMemberNum  = 0;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {

        hd = HistoryData.getInstance(new Date(), new URI(baseMemberUri()), HistoryData.CREATED);
        ce = new Modification();
        initChangeEvent(ce);
        ce.getExtendedProperties().put(TRSUtil.dateModifiedQname, new Date());
    }

    @Test
    public final void testHistoryDataToChangeEvent() {
        ChangeEvent convertedCe = TRSTestUtil.historyDataToChangeEvent(hd);
        HistoryData convertedHd = TRSUtil.changeEventToHistoryData(ce);

        Assert.assertTrue(convertedCe.getChanged().equals(hd.getUri()));
        Assert.assertTrue(convertedCe.getExtendedProperties()
                                     .get(TRSUtil.dateModifiedQname)
                                     .equals(hd.getTimestamp()));
        Assert.assertTrue(convertedCe instanceof Creation);

        Assert.assertTrue(ce.getChanged().equals(convertedHd.getUri()));
        Assert.assertTrue(ce.getExtendedProperties()
                            .get(TRSUtil.dateModifiedQname)
                            .equals(convertedHd.getTimestamp()));
        Assert.assertTrue(convertedHd.getType().equals(HistoryData.MODIFIED));

    }

    private static void initChangeEvent(ChangeEvent ce) throws URISyntaxException {
        java.net.URI changeEvenUri = new URI(changeEventUri());
        java.net.URI baseMemberUri = new URI(baseMemberUri());

        ce.setAbout(changeEvenUri);
        ce.setChanged(baseMemberUri);
        ce.setOrder(changeEventNum);

    }

    private static String changeEventUri() {
        return ceUriPrefix + "/" + changeEventNum++;
    }

    private static String baseMemberUri() {
        return ceUriPrefix + "/" + baseMemberNum++;
    }

}
