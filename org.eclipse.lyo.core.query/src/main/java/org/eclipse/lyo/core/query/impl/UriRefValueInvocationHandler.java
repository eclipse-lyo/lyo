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

import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.UriRefValue;
import org.eclipse.lyo.core.query.Value.Type;

/**
 * Proxy implementation of {@link UriRefValue} interface
 */
class UriRefValueInvocationHandler extends ValueInvocationHandler
{
    public
    UriRefValueInvocationHandler(CommonTree tree)
    {
        super(tree, Type.URI_REF);
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
        String methodName = method.getName();
        boolean isValue = methodName.equals("value");
        
        if (! isValue &&
            ! methodName.equals("toString")) {
            super.invoke(proxy, method, args);
        }
        
        if (value == null) {
            
            String rawValue = tree.getText();
            
            // XXX - determine if need to unescape
            value = rawValue.substring(1, rawValue.length() - 1);
        }
        
        if (isValue) {
            return value;
        }
        
        return '<' + value + '>';
    }
    
    private String value = null;
}
