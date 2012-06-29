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

import org.eclipse.lyo.core.query.Properties;
import org.eclipse.lyo.core.query.Properties.Type;

/**
 * Proxy implementation of {@link Properties} interface
 */
class PropertiesInvocationHandler implements InvocationHandler
{
    protected
    PropertiesInvocationHandler(Type type)
    {
        this.type = type;
    }
    
    /**
     * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    @Override
    public Object invoke(
        Object proxy,
        Method method,
        Object[] args
    ) throws Throwable
    {
        return type;
    }
    
    private final Type type;
}
