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
import javax.servlet.http.HttpServletRequest;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import static org.mockito.Mockito.when;

public class OSLC4JUtilsTest {

    @Before
    public void clearPublicURISystemProperty() throws MalformedURLException {
        System.setProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION, "true");
        OSLC4JUtils.setPublicURI(null);
        OSLC4JUtils.setServletPath(null);
    }

    @Test
    @SuppressWarnings("deprecation")
    public void testPublicURI() throws MalformedURLException {
        HttpServletRequest mockedRequest = Mockito.mock(HttpServletRequest.class);
        Mockito.when(mockedRequest.getServletPath()).thenReturn("/resources");
        Mockito.when(mockedRequest.getPathInfo()).thenReturn("/bugs/1");
        OSLC4JUtils.setPublicURI("http://hostname.example.com:12357/myapp/");
        String resolvedUri = OSLC4JUtils.resolveURI(mockedRequest, true);
        assertEquals("http://hostname.example.com:12357/myapp/resources/bugs/1", resolvedUri);
    }

    @Test
    @SuppressWarnings("deprecation")
    public void testDisableHostResolution() {
        HttpServletRequest mockedRequest = mockRequest();
        String resolvedUri = OSLC4JUtils.resolveURI(mockedRequest, true);
        assertEquals("https://hostname.example.com:12357/myapp/resources/bugs/1", resolvedUri);
    }

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
    public void resolveFullPathUriWithPublicUri() throws Exception {
        OSLC4JUtils.setPublicURI("http://a.b/");
        OSLC4JUtils.setServletPath("/abc");
        final HttpServletRequest request = mockRequest();

        final String fullUri = OSLC4JUtils.resolveFullUri(request);

        assertEquals("http://a.b/abc/bugs/1", fullUri);
    }

    @Test
    public void resolveFullPathUriWithoutPublicUri() {
        final HttpServletRequest request = mockRequest();

        final String fullUri = OSLC4JUtils.resolveFullUri(request);

        assertEquals("https://hostname.example.com:12357/myapp/resources/bugs/1", fullUri);
    }

    @Test
    public void resolveServletUriWithPublicUri() throws Exception {
        OSLC4JUtils.setPublicURI("http://a.b/");
        OSLC4JUtils.setServletPath("/abc");
        final HttpServletRequest request = mockRequest();

        final String fullUri = OSLC4JUtils.resolveServletUri(request);

        assertEquals("http://a.b/abc", fullUri);
    }

    @Test
    public void resolveServletUriWithoutPublicUri() {
        final HttpServletRequest request = mockRequest();

        final String fullUri = OSLC4JUtils.resolveServletUri(request);

        assertEquals("https://hostname.example.com:12357/myapp/resources", fullUri);
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

    private HttpServletRequest mockRequest() {
        HttpServletRequest mockedRequest = Mockito.mock(HttpServletRequest.class);
        Mockito.when(mockedRequest.getScheme()).thenReturn("https");
        Mockito.when(mockedRequest.getServerName()).thenReturn("hostname.example.com");
        Mockito.when(mockedRequest.getServerPort()).thenReturn(12357);
        Mockito.when(mockedRequest.getContextPath()).thenReturn("/myapp");
        Mockito.when(mockedRequest.getServletPath()).thenReturn("/resources");
        Mockito.when(mockedRequest.getPathInfo()).thenReturn("/bugs/1");
        return mockedRequest;
    }
}
