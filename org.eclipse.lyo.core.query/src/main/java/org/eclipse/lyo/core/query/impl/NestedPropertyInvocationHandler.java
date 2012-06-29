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
import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.OslcSelectParser;
import org.eclipse.lyo.core.query.Properties;
import org.eclipse.lyo.core.query.Property.Type;
import org.eclipse.lyo.core.query.PropertyList;
import org.eclipse.lyo.core.query.Wildcard;

/**
 * Proxy implementation of {@link NestedProperty} interface
 */
class NestedPropertyInvocationHandler extends PropertyInvocationHandler
{
    public
    NestedPropertyInvocationHandler(
        CommonTree tree,
        Map<String, String> prefixMap
    )
    {
        super((CommonTree)tree.getChild(0).getChild(0), Type.NESTED_PROPERTY,
              prefixMap);
        
        this.tree = tree;
    }

    /**
     * @see org.eclipse.lyo.core.query.impl.PropertyInvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
     */
    @Override
    public Object invoke(Object proxy, Method method, Object[] args)
        throws Throwable
    {
        String methodName = method.getName();
        boolean isProperties = methodName.equals("properties");
        
        if (! isProperties &&
            ! methodName.equals("toString")) {
            return super.invoke(proxy, method, args);
        }
        
        if (isProperties && properties != null) {
            return properties;            
        }
        
        CommonTree treeProperties = (CommonTree)tree.getChild(1);
        
        switch (treeProperties.getType())
        {
        case OslcSelectParser.WILDCARD:
            properties = (Properties)
                Proxy.newProxyInstance(Wildcard.class.getClassLoader(), 
                        new Class<?>[] { Wildcard.class },
                        new WildcardInvocationHandler());
            break;
        default:
        case OslcSelectParser.PROPERTIES:
            properties = (Properties)
                Proxy.newProxyInstance(PropertyList.class.getClassLoader(), 
                        new Class<?>[] { PropertyList.class },
                        new PropertyListInvocationHandler(treeProperties,
                                                            prefixMap));
            break;
        }
        
        if (isProperties) {
            return properties;
        }
        
        return ((NestedProperty)proxy).identifier().toString() + '{' +
            properties.toString() + '}';
    }
    
    private final CommonTree tree;
    private Properties properties = null;
}
