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
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.NestedProperty;
import org.eclipse.lyo.core.query.OslcSelectParser;
import org.eclipse.lyo.core.query.Property;
import org.eclipse.lyo.core.query.Property.Type;

/**
 * Proxy implementation of {@link NestedProperty} interface
 */
class NestedPropertyInvocationHandler extends PropertyInvocationHandler
{
	public
	NestedPropertyInvocationHandler(
		Tree tree,
		Map<String, String> prefixMap
	)
	{
		super((CommonTree)tree.getChild(0).getChild(0), Type.NESTED_PROPERTY,
			  prefixMap, tree.getChild(0).getType() == OslcSelectParser.WILDCARD);
		
		this.tree = tree;
	}

	/**
	 * @see org.eclipse.lyo.core.query.impl.PropertyInvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
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
		
		children = PropertiesInvocationHandler.createChildren(
				(CommonTree)tree.getChild(1), prefixMap);		 
		
		if (isChildren) {
			return children;
		}
		
		NestedProperty nestedProperty = ((NestedProperty)proxy);
		StringBuffer buffer = new StringBuffer();
		
		buffer.append(nestedProperty.isWildcard() ?
							"*" :
							nestedProperty.identifier().toString());
		buffer.append('{');
		
		PropertiesInvocationHandler.childrenToString(buffer, children);
		
		buffer.append('}');
		
		return buffer.toString();
	}
	
	private final Tree tree;
	private List<Property> children = null;
}
