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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;

/**
 * Defines the global namespace prefix mappings.
 * The global namespace mappings do not take precedence
 * over any other namespace mappings definition, which means
 * that in case of conflict the {@link OslcSchema} will
 * override the prefix.
 * <p>
 * This class is a singleton instance, that can be obtained
 * calling {@link #getInstance()}, since it works with
 * any request even if there are no annotation mapping.
 *
 * @author Daniel Figueiredo Caetano
 */
public class OslcGlobalNamespaceProvider {

    private static OslcGlobalNamespaceProvider instance;

    private Map<String, String> prefixDefinitionMap;

    /**
     * Private construct for singleton pattern.
     */
    private OslcGlobalNamespaceProvider() {
        this.prefixDefinitionMap = new HashMap<>();
    }

    /**
     * Gets the unique instance of this class.
     *
     * @return singleton class instance.
     */
    public static synchronized OslcGlobalNamespaceProvider getInstance() {
        if (null == instance) {
            instance = new OslcGlobalNamespaceProvider();
        }
        return instance;
    }

    /**
     * Gets the Global namespace mappings, these mappings are
     * applied to all operations, even without the annotation
     * mappings.
     * <p>
     * key	 - prefix
     * value - namespace
     *
     * @return empty hash map instance if there are no global
     * namespace mappings.
     */
    public Map<String, String> getPrefixDefinitionMap() {
        return prefixDefinitionMap;
    }

    /**
     * Sets the global prefix definition map with the given map.
     * Note that this operation overrides the current map.
     *
     * @param prefixDefinitionMap that will replace the current.
     */
    public void setPrefixDefinitionMap(Map<String, String> prefixDefinitionMap) {
        if (null == prefixDefinitionMap) {
            this.prefixDefinitionMap.clear();
        } else {
            this.prefixDefinitionMap = prefixDefinitionMap;
        }
    }

}
