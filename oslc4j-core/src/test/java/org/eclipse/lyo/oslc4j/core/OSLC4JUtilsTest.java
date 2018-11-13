package org.eclipse.lyo.oslc4j.core;

import java.net.MalformedURLException;
import javax.servlet.http.HttpServletRequest;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import static org.mockito.Mockito.when;

/**
 * Created on 2017-06-01
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public class OSLC4JUtilsTest {

    @Before
    public void clearPublicURISystemProperty() throws MalformedURLException {
//        System.clearProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION);
        System.setProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION, "true");
        OSLC4JUtils.setPublicURI(null);
        OSLC4JUtils.setServletPath(null);
    }

    @Test
    public void testPublicURI() throws MalformedURLException {
        HttpServletRequest mockedRequest = Mockito.mock(HttpServletRequest.class);
        when(mockedRequest.getServletPath()).thenReturn("/resources");
        when(mockedRequest.getPathInfo()).thenReturn("/bugs/1");
        OSLC4JUtils.setPublicURI("http://hostname.example.com:12357/myapp/");
        String resolvedUri = OSLC4JUtils.resolveURI(mockedRequest, true);
        assertEquals("http://hostname.example.com:12357/myapp/resources/bugs/1", resolvedUri);
    }

    @Test
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
    public void resolveFullPathUriWithoutPublicUri() throws Exception {
        final HttpServletRequest request = mockRequest();

        final String fullUri = OSLC4JUtils.resolveFullUri(request);

        assertEquals("https://hostname.example.com:12357/myapp/resources/bugs/1", fullUri);
    }

    @Test
    public void resolveServletUriWithPublicUri() throws Exception {
        OSLC4JUtils.setPublicURI("http://a.b/");
        OSLC4JUtils.setServletPath("/abc");
        // request.getServletPath() -> "/resources"
        final HttpServletRequest request = mockRequest();

        final String fullUri = OSLC4JUtils.resolveServletUri(request);

        assertEquals("http://a.b/abc", fullUri);
    }

    @Test
    public void resolveServletUriWithoutPublicUri() throws Exception {
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
