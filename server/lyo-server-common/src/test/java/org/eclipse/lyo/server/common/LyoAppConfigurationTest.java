package org.eclipse.lyo.server.common;

import jakarta.servlet.ServletContext;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.when;

public class LyoAppConfigurationTest {

    private static final String TEST_KEY = "test.property";
    private static final String SYSTEM_VAL = "systemValue";
    private static final String CONTEXT_VAL = "contextValue";

    @Before
    public void setUp() {
        System.clearProperty(TEST_KEY);
    }

    @After
    public void tearDown() {
        System.clearProperty(TEST_KEY);
    }

    @Test
    public void testSystemPropertyOverride() {
        System.setProperty(TEST_KEY, SYSTEM_VAL);
        ServletContext context = Mockito.mock(ServletContext.class);
        when(context.getInitParameter(TEST_KEY)).thenReturn(CONTEXT_VAL);

        String value = LyoAppConfiguration.getOslcConfigProperty(context, TEST_KEY);
        assertEquals("System property should override context param", SYSTEM_VAL, value);
    }

    @Test
    public void testContextFallback() {
        ServletContext context = Mockito.mock(ServletContext.class);
        when(context.getInitParameter(TEST_KEY)).thenReturn(CONTEXT_VAL);

        String value = LyoAppConfiguration.getOslcConfigProperty(context, TEST_KEY);
        assertEquals("Should fall back to context param", CONTEXT_VAL, value);
    }

    @Test
    public void testNotFound() {
        ServletContext context = Mockito.mock(ServletContext.class);
        when(context.getInitParameter(TEST_KEY)).thenReturn(null);

        String value = LyoAppConfiguration.getOslcConfigProperty(context, TEST_KEY);
        assertNull("Should return null if not found", value);
    }

    @Test
    public void testNullContext() {
        System.setProperty(TEST_KEY, SYSTEM_VAL);
        String value = LyoAppConfiguration.getOslcConfigProperty(null, TEST_KEY);
        assertEquals("Should work with null context", SYSTEM_VAL, value);
    }
}
