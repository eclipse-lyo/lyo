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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.InTerm;
import org.eclipse.lyo.core.query.SimpleTerm.Type;
import org.eclipse.lyo.core.query.Value;

/**
 * Proxy implementation of {@link InTerm} interface
 */
class InTermInvocationHandler extends SimpleTermInvocationHandler
{
	public
	InTermInvocationHandler(
		Tree tree,
		Map<String, String> prefixMap
   )
	{
		super(tree, Type.IN_TERM, prefixMap);
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
		boolean isValues = methodName.equals("values");
		
		if (! isValues &&
			! methodName.equals("toString")) {
			return super.invoke(proxy, method, args);
		}
		
		if (values == null) {
			
			Tree currentTree = tree.getChild(1);
			
			values = new ArrayList<Value>(currentTree.getChildCount() - 1);
			
			for (int index = 0; index < currentTree.getChildCount(); index++) {
				
				Tree treeValue = currentTree.getChild(index);
				
				Value value =
					ComparisonTermInvocationHandler.createValue(
							treeValue, "unspported literal value type",
							prefixMap);
				
				values.add(value);
			}
			
			values = Collections.unmodifiableList(values);
		}
		
		if (isValues) {
			return values;
		}
		
		StringBuffer buffer = new StringBuffer();
		
		buffer.append(((InTerm)proxy).property().toString());
		buffer.append(" in [");
		
		boolean first = true;
		
		for (Value value : values) {
			
			if (first) {
				first = false;
			} else {
				buffer.append(',');
			}
			
			buffer.append(value.toString());
		}
		
		buffer.append(']');
		
		return buffer.toString();
	}
	
	private List<Value> values = null;
}
