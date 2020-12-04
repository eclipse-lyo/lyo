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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.CompoundTerm;
import org.eclipse.lyo.core.query.InTerm;
import org.eclipse.lyo.core.query.OslcWhereParser;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.SimpleTerm.Type;

/**
 * Proxy implementation of {@link CompoundTerm} interface
 */
public class CompoundTermInvocationHandler extends SimpleTermInvocationHandler
{
	public
	CompoundTermInvocationHandler(
		Tree tree,
		boolean isTopLevel,
		Map<String, String> prefixMap
	)
	{
		super(isTopLevel ? null : tree,
			  isTopLevel ? Type.TOP_LEVEL : Type.NESTED,
			  prefixMap);
		
		this.tree = tree;
		this.isTopLevel = isTopLevel;
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
		boolean isChildren = methodName.equals("children");
		
		if (! isChildren &&
			! methodName.equals("toString")) {
			return super.invoke(proxy, method, args);
		}
		
		if (children != null) {
			return children;
		}
		
		Tree currentTree =
			isTopLevel ?
				tree :
				tree.getChild(1);
		
		children =
			new ArrayList<SimpleTerm>(
					currentTree.getChildCount() - (isTopLevel ? 0 : 1));
		
		for (int index = 0; index < currentTree.getChildCount(); index++) {
			
			Tree child = currentTree.getChild(index);
			
			Object simpleTerm;
			
			switch(child.getType()) {
			case OslcWhereParser.SIMPLE_TERM:
				simpleTerm = 
					Proxy.newProxyInstance(ComparisonTerm.class.getClassLoader(), 
							new Class<?>[] { ComparisonTerm.class },
							new ComparisonTermInvocationHandler(
									child, prefixMap));
				break;
			case OslcWhereParser.IN_TERM:
				simpleTerm = 
					Proxy.newProxyInstance(InTerm.class.getClassLoader(), 
							new Class<?>[] { InTerm.class },
							new InTermInvocationHandler(
									child, prefixMap));
				break;
			case OslcWhereParser.COMPOUND_TERM:
				simpleTerm = 
					Proxy.newProxyInstance(CompoundTerm.class.getClassLoader(), 
							new Class<?>[] { CompoundTerm.class },
							new CompoundTermInvocationHandler(
									child, false, prefixMap));
				break;
			default:
				throw new IllegalStateException("unimplemented type of simple term: " + child.getText());
			}
			
			children.add((SimpleTerm)simpleTerm);
		}
		
		children = Collections.unmodifiableList(children);
		
		if (isChildren) {
			return children;
		}
		
		StringBuffer buffer = new StringBuffer();
		
		if (! isTopLevel) {
			buffer.append(((CompoundTerm)proxy).property().toString());
			buffer.append('{');
		}
		
		boolean first = true;
		
		for (SimpleTerm term : children) {
			
			if (first) {
				first = false;
			} else {
				buffer.append(" and ");
			}
			
			buffer.append(term.toString());
		}
		
		if (! isTopLevel) {
			buffer.append('}');
		}
		
		return buffer.toString();
	}
	
	private final Tree tree;
	private final boolean isTopLevel;
	private List<SimpleTerm> children = null;
}
