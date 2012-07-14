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
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.Identifier;
import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.OslcSelectParser;
import org.eclipse.lyo.core.query.Properties;
import org.eclipse.lyo.core.query.Property;
import org.eclipse.lyo.core.query.Property.Type;
import org.eclipse.lyo.core.query.Wildcard;

/**
 * Proxy implementation of {@link Properties} interface
 */
public class PropertiesInvocationHandler implements InvocationHandler
{
    public
    PropertiesInvocationHandler(
        CommonTree tree,
        Map<String, String> prefixMap
    )
    {
        this.tree = tree;
        this.prefixMap = prefixMap;
    }
    
    /**
     * Construct a {@link Properties} proxy that has a single
     * {@link Wildcard} child
     */
    public
    PropertiesInvocationHandler()
    {
        this.tree = null;
        this.prefixMap = null;
        
        children = new ArrayList<Property>(1);
        
        children.add((Property)Proxy.newProxyInstance(
                        Wildcard.class.getClassLoader(), 
                        new Class<?>[] { Wildcard.class },
                        new WildcardInvocationHandler()));
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
        boolean isChildren = method.getName().equals("children");
        
        if (isChildren && children != null) {
            return children;
        }
        
        children = createChildren(tree, prefixMap);
        
        if (isChildren) {
            return children;
        }
        
        StringBuffer buffer = childrenToString(new StringBuffer(), children);
        
        return buffer.toString();
    }
    
    /**
     * Generate a list of property children from a parse tree node
     * 
     * @param tree
     * @param prefixMap
     * 
     * @return the resulting property list
     */
    static List<Property>
    createChildren(
        CommonTree tree,
        Map<String, String> prefixMap
    )
    {
        @SuppressWarnings("unchecked")
        List<CommonTree> treeChildren = tree.getChildren();        
        List<Property> children = new ArrayList<Property>(treeChildren.size());
        
        for (CommonTree treeChild : treeChildren) {
            
            Property property;
            
            switch (treeChild.getType())
            {
            case OslcSelectParser.WILDCARD:
                property = (Property)
                    Proxy.newProxyInstance(Wildcard.class.getClassLoader(), 
                            new Class<?>[] { Wildcard.class },
                            new WildcardInvocationHandler());
                break;
            case OslcSelectParser.PREFIXED_NAME:
                property = (Property)
                    Proxy.newProxyInstance(Identifier.class.getClassLoader(), 
                            new Class<?>[] { Identifier.class },
                            new PropertyInvocationHandler(
                                    (CommonTree)treeChild.getChild(0),
                                    Type.IDENTIFIER, prefixMap, false));
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
        
        return children;
    }
    
    /**
     * Generate string representation of a children property list
     * 
     * @param buffer
     * @param children
     * 
     * @return the buffer representation of the property list
     */
    static StringBuffer
    childrenToString(
        StringBuffer buffer,
        List<Property> children
    )
    {
        boolean first = true;
         
         for (Property property : children) {
             
             if (first) {
                 first = false;
             } else {
                 buffer.append(',');
             }
             
             buffer.append(property.toString());
         }
        
         return buffer;
    }
    
    private final CommonTree tree;
    protected final Map<String, String> prefixMap;
    private List<Property> children = null;
}
