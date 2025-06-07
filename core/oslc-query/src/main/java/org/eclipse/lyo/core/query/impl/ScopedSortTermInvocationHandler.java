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
import java.lang.reflect.Proxy;
import java.util.Map;
import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.ScopedSortTerm;
import org.eclipse.lyo.core.query.SortTerm.Type;
import org.eclipse.lyo.core.query.SortTerms;

/**
 * Proxy implementation of {@link ScopedSortTerm}
 */
class ScopedSortTermInvocationHandler extends SortTermInvocationHandler {
    public ScopedSortTermInvocationHandler(Tree tree, Map<String, String> prefixMap) {
        super(Type.SCOPED, tree, prefixMap);
    }

    /* (non-Javadoc)
     * @see org.eclipse.lyo.core.query.impl.SortTerm#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String methodName = method.getName();
        boolean isSortTerm = methodName.equals("sortTerms");

        if (!isSortTerm && !methodName.equals("toString")) {
            return super.invoke(proxy, method, args);
        }

        if (sortTerms == null) {

            sortTerms =
                    (SortTerms)
                            Proxy.newProxyInstance(
                                    SortTerms.class.getClassLoader(),
                                    new Class<?>[] {SortTerms.class},
                                    new SortTermsInvocationHandler(tree.getChild(1), prefixMap));
        }

        if (isSortTerm) {
            return sortTerms;
        }

        StringBuffer buffer = new StringBuffer();

        buffer.append(((ScopedSortTerm) proxy).identifier().toString());
        buffer.append('{');
        buffer.append(sortTerms.toString());
        buffer.append('}');

        return buffer.toString();
    }

    private SortTerms sortTerms = null;
}
