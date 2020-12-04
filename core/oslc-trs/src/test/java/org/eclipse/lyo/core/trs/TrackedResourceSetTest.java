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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */

package org.eclipse.lyo.core.trs;

import org.apache.jena.rdf.model.Model;
import java.net.URI;
import java.net.URISyntaxException;
import org.eclipse.lyo.oslc4j.provider.jena.JenaModelHelper;
import static org.junit.Assert.*;
import static org.assertj.core.api.Assertions.*;
import org.junit.Test;

/**
 * TRS resource unit tests with JenaModelHelper.
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 2.2.0
 */
public class TrackedResourceSetTest {
    @Test
    public void emptyTrsIsUnmarshalled() throws Exception {
        final ChangeLog changeLog = new ChangeLog();
        changeLog.setAbout(URI.create("http://example.com/dummy/changelog"));

        final TrackedResourceSet trsExpected = aTrsWithChangelog(changeLog);

        final Model model = JenaModelHelper.createJenaModel(new Object[]{trsExpected});
        final Object[] objects = JenaModelHelper.fromJenaModel(model, TrackedResourceSet.class);
        final TrackedResourceSet trsJena = (TrackedResourceSet) objects[0];

        assertEquals("TRS Base URI is preserved", trsExpected.getBase(), trsJena.getBase());
        assertEquals("TRS Changelog About URI is preserved", trsExpected.getChangeLog().getAbout(),
                trsJena.getChangeLog().getAbout());
    }

    @Test
    public void simpleTrsIsUnmarshalled() throws Exception {
        final ChangeLog changeLog = new ChangeLog();
        changeLog.setAbout(URI.create("http://example.com/dummy/changelog"));
        changeLog.getChange().add(new Modification(URI.create("http://example.com/dummy/MOD-001"),
                URI.create("http://example.com/dummy/RES-001"), 543));

        final TrackedResourceSet trsExpected = aTrsWithChangelog(changeLog);

        final Model model = JenaModelHelper.createJenaModel(new Object[]{trsExpected});
        final Object[] objects = JenaModelHelper.fromJenaModel(model, TrackedResourceSet.class);
        final TrackedResourceSet trsJena = (TrackedResourceSet) objects[0];

        assertEquals("TRS Base URI is preserved", trsExpected.getBase(), trsJena.getBase());
        assertEquals("TRS Changelog About URI is preserved", trsExpected.getChangeLog().getAbout(),
                trsJena.getChangeLog().getAbout());
        assertThat(trsJena.getChangeLog().getChange()).hasSize(
                trsExpected.getChangeLog().getChange().size());
    }

    private TrackedResourceSet aTrsWithChangelog(final ChangeLog changeLog)
            throws URISyntaxException {
        final TrackedResourceSet trsExpected = new TrackedResourceSet();
        trsExpected.setBase(URI.create("http://example.com/dummy"));
        trsExpected.setChangeLog(changeLog);
        return trsExpected;
    }
}
