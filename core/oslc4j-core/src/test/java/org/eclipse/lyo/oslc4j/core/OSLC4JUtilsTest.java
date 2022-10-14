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
package org.eclipse.lyo.oslc4j.core;

import java.net.MalformedURLException;
import static org.junit.Assert.*;
import org.junit.Test;

public class OSLC4JUtilsTest {

    @Test(expected = IllegalArgumentException.class)
    public void setServletPathNull() throws Exception {
        OSLC4JUtils.setPublicURI(null);
        OSLC4JUtils.setServletPath("/abc");
    }

    @Test
    public void setServletPathTwoSlashes() throws Exception {
        OSLC4JUtils.setPublicURI("http://a.b/");
        OSLC4JUtils.setServletPath("/abc");

        final String servletURI = OSLC4JUtils.getServletURI();

        assertEquals("http://a.b/abc", servletURI);
    }

    @Test(expected = MalformedURLException.class)
    public void setServletPathSpaces() throws Exception {
        OSLC4JUtils.setPublicURI("    ");
        OSLC4JUtils.setServletPath("/abc");

        final String servletURI = OSLC4JUtils.getServletURI();

        assertEquals("http://a.b/abc", servletURI);
    }

    @Test
    public void storePagingSafeByDefault() {
        System.clearProperty(OSLC4JConstants.LYO_STORE_PAGING_UNSAFE);
        assertFalse(OSLC4JUtils.isLyoStorePagingUnsafe());
    }

    @Test
    public void storePagingUnsafeSetViaProperty() {
        System.clearProperty(OSLC4JConstants.LYO_STORE_PAGING_UNSAFE);
        assertFalse(OSLC4JUtils.isLyoStorePagingUnsafe());

        System.setProperty(OSLC4JConstants.LYO_STORE_PAGING_UNSAFE, "true");
        assertTrue(OSLC4JUtils.isLyoStorePagingUnsafe());
    }

    @Test
    public void storePagingUnsafeSetViaSetter() {
        System.setProperty(OSLC4JConstants.LYO_STORE_PAGING_UNSAFE, "true");
        assertTrue(OSLC4JUtils.isLyoStorePagingUnsafe());

        OSLC4JUtils.setLyoStorePagingUnsafe(false);
        assertFalse(OSLC4JUtils.isLyoStorePagingUnsafe());
    }


    @Test
    public void storePagingNplus1LimitOffByDefault() {
        System.clearProperty(OSLC4JConstants.LYO_STORE_PAGING_PRECISE_LIMIT);
        assertTrue(OSLC4JUtils.hasLyoStorePagingPreciseLimit());
    }

    @Test
    public void storePagingNplus1LimitSetViaProperty() {
        System.clearProperty(OSLC4JConstants.LYO_STORE_PAGING_PRECISE_LIMIT);
        assertTrue(OSLC4JUtils.hasLyoStorePagingPreciseLimit());

        System.setProperty(OSLC4JConstants.LYO_STORE_PAGING_PRECISE_LIMIT, "false");
        assertFalse(OSLC4JUtils.hasLyoStorePagingPreciseLimit());
    }

    @Test
    public void storePagingNplus1LimitSetViaSetter() {
        System.setProperty(OSLC4JConstants.LYO_STORE_PAGING_PRECISE_LIMIT, "true");
        assertTrue(OSLC4JUtils.hasLyoStorePagingPreciseLimit());

        OSLC4JUtils.setLyoStorePagingPreciseLimit(false);
        assertFalse(OSLC4JUtils.hasLyoStorePagingPreciseLimit());
    }

}
