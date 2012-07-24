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
import org.eclipse.lyo.core.query.LangedStringValue;
import org.eclipse.lyo.core.query.Value.Type;

/**
 * Proxy implementation of {@link LangedStringValue} interface
 */
class LangedStringValueInvocationHandler extends ValueInvocationHandler
{
    public
    LangedStringValueInvocationHandler(CommonTree tree)
    {
        super(tree, Type.LANGED_STRING);
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
            ! methodName.equals("langTag")&&
            ! methodName.equals("toString")) {
            return super.invoke(proxy, method, args);
        }
        
        if (isValue) {
            
            if (value == null) {
                
                String rawValue = ((CommonTree)tree.getChild(0)).getText();
                
                // XXX - determine if need to unescape
                value = rawValue.substring(1, rawValue.length() - 1);
            }
            
            return value;
        }
        
        if (langTag == null) {
            langTag = ((CommonTree)tree.getChild(1)).getText().substring(1);
        }
    
        if (methodName.equals("langTag")) {
            return langTag;
        }
        
        return '"' + ((LangedStringValue)proxy).value() + "\"@" +
            ((LangedStringValue)proxy).langTag();
    }
    
    private String value = null;
    private String langTag = null;
}
