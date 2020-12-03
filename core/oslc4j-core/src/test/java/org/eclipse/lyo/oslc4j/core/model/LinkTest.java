/*******************************************************************************
 * Copyright (c) 2017 KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents /edl-v10.php.
 *
 * Contributors:
 *
 *	   Andrew Berezovskyi - basic test code
 *******************************************************************************/

package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.*;
import org.junit.Test;

public class LinkTest {

    private static final String URI_A = "https://example.com/I0tnj0LX";
    private static final String URI_B = "https://example.com/chKY439J";
    private static final String LABEL_A = "Example label A";
    private static final String LABEL_B = "Example label B";

    @Test
    public void testEqualsWorksOnIdenticalValues() throws Exception {
        String label = "Example label";

        // use new String to force unequal references
        Link linkA = new Link(URI.create(URI_A), new String(label));
        Link linkB = new Link(URI.create(URI_A), new String(label));

        assertEquals(linkA, linkB);
    }

    @Test
    public void testEqualsWorksOnDifferentLabels() throws Exception {
        // use new String to force unequal references
        Link linkA = new Link(URI.create(URI_A), new String(LABEL_A));
        Link linkB = new Link(URI.create(URI_A), new String(LABEL_B));

        assertEquals(linkA, linkB);
    }

    @Test
    public void testEqualsWorksInHashSet() throws Exception {

        Set<Link> setA = new HashSet<Link>() {{
            this.add(new Link(URI.create(URI_A), LABEL_A));
            this.add(new Link(URI.create(URI_B), LABEL_B));
        }};

        Set<Link> setB = new HashSet<Link>() {{
            this.add(new Link(URI.create(URI_B), LABEL_B));
            this.add(new Link(URI.create(URI_A), LABEL_A));
        }};

        assertThat(setA, equalTo(setB));
    }

    @Test
    public void testEqualsWorksInHashSetWhenDifferent() throws Exception {

        Set<Link> setA = new HashSet<Link>() {{
            this.add(new Link(URI.create(URI_A), LABEL_A));
            this.add(new Link(URI.create(URI_B), LABEL_B));
        }};

        Set<Link> setB = new HashSet<Link>() {{
            this.add(new Link(URI.create(URI_A)));
        }};

        assertThat(setA, not(equalTo(setB)));
    }

    @Test
    public void testHashCodeSameEquals() throws Exception {
        Link linkA = new Link(URI.create(URI_A), new String(LABEL_A));
        Link linkB = new Link(URI.create(URI_A), new String(LABEL_A));

        assertThat(linkA.hashCode(), equalTo(linkB.hashCode()));
    }

    @Test
    public void testHashCodeDiffers() throws Exception {
        Link linkA = new Link(URI.create(URI_A), new String(LABEL_A));
        Link linkB = new Link(URI.create(URI_B), new String(LABEL_A));

        assertThat(linkA.hashCode(), not(equalTo(linkB.hashCode())));
    }

    @Test
    public void testHashCodeIsTheSameForDifferentLabels() throws Exception {
        Link linkA = new Link(URI.create(URI_A), new String(LABEL_A));
        Link linkB = new Link(URI.create(URI_A), new String(LABEL_B));

        assertThat(linkA.hashCode(), equalTo(linkB.hashCode()));
    }
}
