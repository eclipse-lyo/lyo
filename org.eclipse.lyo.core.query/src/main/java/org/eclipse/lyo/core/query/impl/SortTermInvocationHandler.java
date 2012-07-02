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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.SortTerm;
import org.eclipse.lyo.core.query.SortTerm.Type;

/**
 * Proxy implementation of {@link SortTerm}
 */
class SortTermInvocationHandler implements InvocationHandler
{
    public
    SortTermInvocationHandler(
        Type type,
        CommonTree tree,
        Map<String, String> prefixMap
    )
    {
        this.type = type;
        this.tree = tree;
        this.prefixMap = prefixMap;
    }
    
    /**
     * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    @Override
    public Object
    invoke(
        Object proxy,
        Method method,
        Object[] args
    ) throws Throwable
    {
        if (method.getName().equals("type")) {
            return type;
        }
        
        if (identifier == null) {
            
            String rawProperty = tree.getChild(0).getText();
            
            identifier = new PName();
            
            int colon = rawProperty.indexOf(':');
            
            if (colon < 0) {
                identifier.local = rawProperty;
            } else { 
                if (colon > 0) {
                    identifier.prefix = rawProperty.substring(0, colon);
                    identifier.namespace = prefixMap.get(identifier.prefix);
                }
                identifier.local = rawProperty.substring(colon + 1);
            }
        }
        
        return identifier;
    }

    private final Type type;
    protected final CommonTree tree;
    protected final Map<String, String> prefixMap;
    private PName identifier = null;
}
