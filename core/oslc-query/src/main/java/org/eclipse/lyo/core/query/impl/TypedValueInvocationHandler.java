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

import java.lang.reflect.Method;
import java.util.Map;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.TypedValue;
import org.eclipse.lyo.core.query.Value.Type;

/**
 * Proxy implementation of {@link TypedValue} interface
 */
class TypedValueInvocationHandler extends ValueInvocationHandler {
    public TypedValueInvocationHandler(Tree tree, Map<String, String> prefixMap) {
        super(tree, Type.TYPED_STRING);

        this.prefixMap = prefixMap;
    }

    /**
     * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String methodName = method.getName();
        boolean isValue = methodName.equals("value");

        if (!isValue && !methodName.equals("prefixedName") && !methodName.equals("toString")) {
            return super.invoke(proxy, method, args);
        }

        if (isValue) {

            if (value == null) {

                String rawValue = ((CommonTree) tree.getChild(0)).getText();

                // XXX - determine if need to unescape
                value = rawValue.substring(1, rawValue.length() - 1);
            }

            return value;
        }

        if (prefixedName == null) {

            String rawPName = tree.getChild(1).getText();

            prefixedName = new PName();

            int colon = rawPName.indexOf(':');

            if (colon < 0) {
                prefixedName.local = rawPName;
            } else {
                if (colon > 0) {
                    prefixedName.prefix = rawPName.substring(0, colon);
                    prefixedName.namespace = prefixMap.get(prefixedName.prefix);
                }
                prefixedName.local = rawPName.substring(colon + 1);
            }
        }

        if (methodName.equals("prefixedName")) {
            return prefixedName;
        }

        return '"' + ((TypedValue) proxy).value() + "\"^^" + ((TypedValue) proxy).prefixedName();
    }

    private final Map<String, String> prefixMap;
    private String value = null;
    private PName prefixedName = null;
}
