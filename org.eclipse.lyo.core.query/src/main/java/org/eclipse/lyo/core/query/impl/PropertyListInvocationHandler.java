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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.Identifier;
import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.OslcSelectParser;
import org.eclipse.lyo.core.query.Properties.Type;
import org.eclipse.lyo.core.query.Property;

/**
 * Proxy implementation of {@link PropertyList} interface
 */
public class PropertyListInvocationHandler extends PropertiesInvocationHandler
{
    public
    PropertyListInvocationHandler(
        CommonTree tree,
        Map<String, String> prefixMap
    )
    {
        super(Type.PROPERTY_LIST);
        
        this.tree = tree;
        this.prefixMap = prefixMap;
    }

    /**
     * @see PropertiesInvocationHandler#invoke(Object, Method, Object[])
     */
    @Override
    public Object invoke(
        Object proxy,
        Method method,
        Object[] args
    ) throws Throwable
    {
        String methodName = method.getName();
        boolean isChildren = methodName.equals("children");
        
        if (! isChildren &&
            ! methodName.equals("toString")) {
            return super.invoke(proxy, method, args);
        }
        
        if (isChildren && children != null) {
            return children;
        }
        
        @SuppressWarnings("unchecked")
        List<CommonTree> treeChildren = tree.getChildren();
        
        children = new ArrayList<Property>(treeChildren.size());
        
        for (CommonTree treeChild : treeChildren) {
            
            Property property;
            
            switch (treeChild.getType())
            {
            case OslcSelectParser.PREFIXED_NAME:
                property = (Property)
                    Proxy.newProxyInstance(Identifier.class.getClassLoader(), 
                            new Class<?>[] { Identifier.class },
                            new IndentifierInvocationHandler(
                                    (CommonTree)treeChild.getChild(0),
                                    prefixMap));
                break;
            default:
            case OslcSelectParser.NESTED_PROPERTIES:
                property = (Property)
                    Proxy.newProxyInstance(NestedProperty.class.getClassLoader(), 
                            new Class<?>[] { NestedProperty.class },
                            new NestedPropertyInvocationHandler(treeChild,
                                                                prefixMap));
                break;
            }
            
            children.add(property);
        }
        
        children = Collections.unmodifiableList(children);
        
        if (isChildren) {
            return children;
        }
        
        boolean first = true;
        StringBuffer buffer = new StringBuffer();
        
        for (Property property : children) {
            
            if (first) {
                first = false;
            } else {
                buffer.append(',');
            }
            
            buffer.append(property.toString());
        }
        
        return buffer.toString();
    }
    
    private final CommonTree tree;
    private Map<String, String> prefixMap;
    private List<Property> children = null;
}
 