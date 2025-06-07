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
package org.eclipse.lyo.oslc4j.core.test;

import java.util.HashMap;
import java.util.Map;
import org.eclipse.lyo.oslc4j.core.OslcGlobalNamespaceProvider;
import org.junit.Assert;
import org.junit.Test;

/**
 * Tests the behavior of {@link OslcGlobalNamespaceProvider}.
 *
 * @author Daniel Figueiredo Caetano
 *
 */
public class OslcGlobalNamespaceProviderTest {

    /**
     * Tests the unique application instance of the {@link OslcGlobalNamespaceProvider}.
     */
    @Test
    public void testSingletonInstance() {
        OslcGlobalNamespaceProvider globalNamespaceProvider =
                OslcGlobalNamespaceProvider.getInstance();
        Assert.assertNotNull(
                "Global Namespace instance should not be null.", globalNamespaceProvider);
        OslcGlobalNamespaceProvider secondGlobalNamespaceProvider =
                OslcGlobalNamespaceProvider.getInstance();
        Assert.assertSame(
                "There should be only one instance of the OslcGlobalNamespaceProvider class",
                globalNamespaceProvider,
                secondGlobalNamespaceProvider);
    }

    /**
     * Tests if the map can be set to another new map or to null.
     */
    @Test
    public void testSetNullMap() {
        OslcGlobalNamespaceProvider globalNamespaceProvider =
                OslcGlobalNamespaceProvider.getInstance();
        Assert.assertNotNull(
                "Global Namespace Map should not be null when created.",
                globalNamespaceProvider.getPrefixDefinitionMap());
        globalNamespaceProvider.getPrefixDefinitionMap().put("test", "http://anything.com");
        globalNamespaceProvider.setPrefixDefinitionMap(null);
        Assert.assertNotNull(
                "Global Namespace Map should not be null.",
                globalNamespaceProvider.getPrefixDefinitionMap());
        Assert.assertTrue(
                "Map should be cleared and never set to null.",
                globalNamespaceProvider.getPrefixDefinitionMap().isEmpty());
        Map<String, String> namespaceMappings = new HashMap<>(1);
        namespaceMappings.put("any", "http://any.test.com#");
        globalNamespaceProvider.setPrefixDefinitionMap(namespaceMappings);
        Assert.assertFalse(
                "Global Namespace Map could not be set.",
                globalNamespaceProvider.getPrefixDefinitionMap().isEmpty());
    }
}
