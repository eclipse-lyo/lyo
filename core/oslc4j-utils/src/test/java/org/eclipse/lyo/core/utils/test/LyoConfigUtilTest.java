/*
 * Copyright (c) 2026 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.core.utils.test;

import static org.junit.Assert.*;
import org.eclipse.lyo.core.utils.marshallers.LyoConfigUtil;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import jakarta.servlet.ServletContext;
import org.mockito.Mockito;

public class LyoConfigUtilTest {

    private static final String KEY = "test.prop";
    private static final String JVM_QUALIFIED_KEY = LyoConfigUtil.generatePackageBaseKey(LyoConfigUtilTest.class, KEY);
    private static final String DEFAULT_VALUE = "default";
    private static final String VALUE_1 = "value1";
    private static final String VALUE_2 = "value2";

    @Before
    @After
    public void clearProperties() {
        System.clearProperty(JVM_QUALIFIED_KEY);
        System.clearProperty(KEY);
    }

    @Test
    public void testGetOslcConfigPropertyNoContextDefault() {
        String res = LyoConfigUtil.getOslcConfigPropertyNoContext(KEY, DEFAULT_VALUE, LyoConfigUtilTest.class);
        assertEquals(DEFAULT_VALUE, res);
    }

    @Test
    public void testGetOslcConfigPropertyNoContextJvmQualified() {
        System.setProperty(JVM_QUALIFIED_KEY, VALUE_1);
        String res = LyoConfigUtil.getOslcConfigPropertyNoContext(KEY, DEFAULT_VALUE, LyoConfigUtilTest.class);
        assertEquals(VALUE_1, res);
    }

    @Test
    public void testGetOslcConfigPropertyNoContextJvmFallback() {
        System.setProperty(KEY, VALUE_2);
        String res = LyoConfigUtil.getOslcConfigPropertyNoContext(KEY, DEFAULT_VALUE, LyoConfigUtilTest.class);
        assertEquals(VALUE_2, res);
    }

    @Test
    public void testGetOslcConfigPropertyNoContextJvmPriority() {
        System.setProperty(JVM_QUALIFIED_KEY, VALUE_1);
        System.setProperty(KEY, VALUE_2);
        String res = LyoConfigUtil.getOslcConfigPropertyNoContext(KEY, DEFAULT_VALUE, LyoConfigUtilTest.class);
        assertEquals(VALUE_1, res); // Qualified has priority over fallback
    }

    @Test
    public void testGetOslcConfigPropertyWithContext() {
        ServletContext context = Mockito.mock(ServletContext.class);
        Mockito.when(context.getInitParameter(JVM_QUALIFIED_KEY)).thenReturn(VALUE_1);
        
        String res = LyoConfigUtil.getOslcConfigProperty(KEY, DEFAULT_VALUE, context, LyoConfigUtilTest.class);
        assertEquals(VALUE_1, res);
    }

    @Test
    public void testGetOslcConfigPropertyWithContextFallback() {
        ServletContext context = Mockito.mock(ServletContext.class);
        Mockito.when(context.getInitParameter(KEY)).thenReturn(VALUE_2);
        
        String res = LyoConfigUtil.getOslcConfigProperty(KEY, DEFAULT_VALUE, context, LyoConfigUtilTest.class);
        assertEquals(VALUE_2, res);
    }
}
