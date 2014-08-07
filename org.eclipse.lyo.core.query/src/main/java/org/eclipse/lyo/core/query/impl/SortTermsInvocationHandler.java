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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.OslcOrderByParser;
import org.eclipse.lyo.core.query.ScopedSortTerm;
import org.eclipse.lyo.core.query.SimpleSortTerm;
import org.eclipse.lyo.core.query.SortTerm;

/**
 * Proxy implementation of {@link SortTerms}
 */
public class SortTermsInvocationHandler implements InvocationHandler
{
	public
	SortTermsInvocationHandler(
		CommonTree tree,
		Map<String, String> prefixMap
	)
	{
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
		if (children == null) {
			
			@SuppressWarnings("unchecked")
			List<CommonTree> rawChildren = tree.getChildren();
			
			children = new ArrayList<SortTerm>(rawChildren.size());
			
			for (CommonTree child : rawChildren) {
				
				Object simpleTerm;
				
				switch(child.getToken().getType()) {
				case OslcOrderByParser.SIMPLE_TERM:
					simpleTerm = 
						Proxy.newProxyInstance(SimpleSortTerm.class.getClassLoader(), 
								new Class<?>[] { SimpleSortTerm.class },
								new SimpleSortTermInvocationHandler(
										child, prefixMap));
					break;
				case OslcOrderByParser.SCOPED_TERM:
					simpleTerm = 
						Proxy.newProxyInstance(ScopedSortTerm.class.getClassLoader(), 
								new Class<?>[] { ScopedSortTerm.class },
								new ScopedSortTermInvocationHandler(
										child, prefixMap));
					break;
				default:
					throw new IllegalStateException("unimplemented type of sort term: " + child.getToken().getText());
				}
				
				children.add((SortTerm)simpleTerm);
			}
			
			children = Collections.unmodifiableList(children);
		}
		
		if (method.getName().equals("children")) {		  
			return children;
		}
		
		StringBuffer buffer = new StringBuffer();
		boolean first = true;
		
		for (SortTerm term : children) {
			
			if (first) {
				first = false;
			} else {
				buffer.append(',');
			}
			
			buffer.append(term.toString());
		}
		
		return buffer.toString();
	}

	private final CommonTree tree;
	private final Map<String, String> prefixMap;
	private List<SortTerm> children = null;
}
