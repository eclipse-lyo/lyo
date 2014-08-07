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
 *	  Steve Pitschke - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.core.query.impl;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
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
		CommonTree tree,
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
		
		@SuppressWarnings("unchecked")
		List<CommonTree> treeChildren =
			isTopLevel ?
				tree.getChildren() :
				((CommonTree)tree.getChild(1)).getChildren();
		
		children =
			new ArrayList<SimpleTerm>(
					treeChildren.size() - (isTopLevel ? 0 : 1));
		
		for (CommonTree child : treeChildren) {
			
			Object simpleTerm;
			
			switch(child.getToken().getType()) {
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
				throw new IllegalStateException("unimplemented type of simple term: " + child.getToken().getText());
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
	
	private final CommonTree tree;
	private final boolean isTopLevel;
	private List<SimpleTerm> children = null;
}
