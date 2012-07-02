/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *    Steve Pitschke - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.core.query.impl;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.ScopedSortTerm;
import org.eclipse.lyo.core.query.SortTerm.Type;
import org.eclipse.lyo.core.query.SortTerms;

/**
 * Proxy implementation of {@link ScopedSortTerm}
 */
class ScopedSortTermInvocationHandler extends SortTermInvocationHandler
{
    public
    ScopedSortTermInvocationHandler(
        CommonTree tree,
        Map<String, String> prefixMap
    )
    {
        super(Type.SCOPED, tree, prefixMap);
    }

    /* (non-Javadoc)
     * @see org.eclipse.lyo.core.query.impl.SortTerm#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    @Override
    public Object invoke(
        Object proxy,
        Method method,
        Object[] args
    ) throws Throwable
    {
        String methodName = method.getName();
        boolean isSortTerm = methodName.equals("sortTerms");
        
        if (! isSortTerm &&
            ! methodName.equals("toString")) {
                return super.invoke(proxy, method, args);
        }
        
        if (sortTerms == null) {
            
            sortTerms = (SortTerms)
                Proxy.newProxyInstance(SortTerms.class.getClassLoader(), 
                        new Class<?>[] { SortTerms.class },
                        new SortTermsInvocationHandler(
                                (CommonTree)tree.getChild(1), prefixMap));
            
        }
        
        if (isSortTerm) {
            return sortTerms;
        }
        
        StringBuffer buffer = new StringBuffer();
        
        buffer.append(((ScopedSortTerm)proxy).identifier().toString());
        buffer.append('{');
        buffer.append(sortTerms.toString());
        buffer.append('}');
        
        return buffer.toString();
    }

    private SortTerms sortTerms = null;
}
