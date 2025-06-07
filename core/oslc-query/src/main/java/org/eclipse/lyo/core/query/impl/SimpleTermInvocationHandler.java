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

package org.eclipse.lyo.core.query.impl;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Map;
import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.SimpleTerm.Type;

/**
 * Proxy implementation of {@link SimpleTerm} interface
 */
abstract class SimpleTermInvocationHandler implements InvocationHandler {
    protected SimpleTermInvocationHandler(Tree tree, Type type, Map<String, String> prefixMap) {
        this.tree = tree;
        this.type = type;
        this.prefixMap = prefixMap;
    }

    /**
     * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (method.getName().equals("type")) {
            return type;
        }

        if (property != null) {
            return property;
        }

        if (tree == null) {
            return null;
        }

        String rawProperty = tree.getChild(0).toString();

        property = new PName();

        int colon = rawProperty.indexOf(':');

        if (colon < 0) {
            property.local = rawProperty;
        } else {
            if (colon > 0) {
                property.prefix = rawProperty.substring(0, colon);
                property.namespace = prefixMap.get(property.prefix);
            }
            property.local = rawProperty.substring(colon + 1);
        }

        return property;
    }

    protected final Tree tree;
    protected final Map<String, String> prefixMap;
    private final Type type;
    private PName property = null;
}
