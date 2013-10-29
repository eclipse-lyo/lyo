/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *     Samuel Padgett - initial implementation
 *     Samuel Padgett - fix test problems from cached public URI root
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.test;

import static org.mockito.Mockito.when;
import static org.junit.Assert.*;

import java.net.MalformedURLException;

import javax.servlet.http.HttpServletRequest;

import org.eclipse.lyo.oslc4j.core.OSLC4JConstants;
import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

/**
 * Tests URI resolution by {@link OSLC4JUtils#resolveURI}.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @since 2.0.0
 */
public class ResolveURITest {
	@Before
	public void clearPublicURISystemProperty() throws MalformedURLException {
		System.clearProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION);
		OSLC4JUtils.setPublicURI(null);
	}

	@Test
	public void testPublicURI() throws MalformedURLException {
		 HttpServletRequest  mockedRequest = Mockito.mock(HttpServletRequest.class);
		 when(mockedRequest.getServletPath()).thenReturn("/resources");
		 when(mockedRequest.getPathInfo()).thenReturn("/bugs/1");
		 OSLC4JUtils.setPublicURI("http://hostname.example.com:12357/myapp/");
		 String resolvedUri = OSLC4JUtils.resolveURI(mockedRequest, true);
		 assertEquals("http://hostname.example.com:12357/myapp/resources/bugs/1", resolvedUri);
	}

	@Test
	public void testDisableHostResolution() {
		 HttpServletRequest  mockedRequest = Mockito.mock(HttpServletRequest.class);
		 when(mockedRequest.getScheme()).thenReturn("https");
		 when(mockedRequest.getServerName()).thenReturn("hostname.example.com");
		 when(mockedRequest.getServerPort()).thenReturn(12357);
		 when(mockedRequest.getContextPath()).thenReturn("/myapp");
		 when(mockedRequest.getServletPath()).thenReturn("/resources");
		 when(mockedRequest.getPathInfo()).thenReturn("/bugs/1");
		 System.setProperty(OSLC4JConstants.OSLC4J_DISABLE_HOST_RESOLUTION, "true");
		 String resolvedUri = OSLC4JUtils.resolveURI(mockedRequest, true);
		 assertEquals("https://hostname.example.com:12357/myapp/resources/bugs/1", resolvedUri);
	}
}
