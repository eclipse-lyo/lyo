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
package org.eclipse.lyo.oslc4j.core.test.customnamespace;

/**
 * Common constants used to the test the global and custom namespace.
 *
 * @author Daniel Figueiredo Caetano
 *
 */
public interface CustomNamespaceConstants {

    /*
     * Test prefixes
     */
    String TEST1_PREFIX = "test1";
    String TEST2_PREFIX = "test2";
    String CUSTOM_PREFIX = "custom";
    String GLOBAL_PREFIX = "global";

    /*
     * Test namespaces
     */
    String TEST1_URL = "http://test1.oslc4j.com#";
    String TEST2_URL = "http://test2.oslc4j.com#";
    String CUSTOM_URL = "http://custom.oslc4j.com#";
    String GLOBAL_URL = "http://global.oslc4j.com#";
}
