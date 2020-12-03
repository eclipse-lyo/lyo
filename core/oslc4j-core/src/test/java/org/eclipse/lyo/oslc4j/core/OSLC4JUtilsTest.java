/*!*****************************************************************************
 * Copyright (c) 2017 Andrew Berezovskyi.
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
 *	   Andrew Berezovskyi		- initial API and implementation
 *******************************************************************************/
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
        when(mockedRequest.getServletPath()).thenReturn("/resources");
        when(mockedRequest.getPathInfo()).thenReturn("/bugs/1");
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

    private HttpServletRequest mockRequest() {
        HttpServletRequest mockedRequest = Mockito.mock(HttpServletRequest.class);
        when(mockedRequest.getScheme()).thenReturn("https");
        when(mockedRequest.getServerName()).thenReturn("hostname.example.com");
        when(mockedRequest.getServerPort()).thenReturn(12357);
        when(mockedRequest.getContextPath()).thenReturn("/myapp");
        when(mockedRequest.getServletPath()).thenReturn("/resources");
        when(mockedRequest.getPathInfo()).thenReturn("/bugs/1");
        return mockedRequest;
    }
}
