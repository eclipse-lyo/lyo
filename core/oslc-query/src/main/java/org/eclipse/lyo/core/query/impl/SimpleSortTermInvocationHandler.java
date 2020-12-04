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
import java.util.Map;

import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.SimpleSortTerm;
import org.eclipse.lyo.core.query.SortTerm.Type;

/**
 * @author pitschke
 *
 */
public class SimpleSortTermInvocationHandler extends SortTermInvocationHandler
{
	public
	SimpleSortTermInvocationHandler(
		Tree tree,
		Map<String, String> prefixMap
	)
	{
		super(Type.SIMPLE, tree, prefixMap);
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
		boolean isAscending = methodName.equals("ascending");
		
		if (! isAscending &&
			! methodName.equals("toString")) {
			return super.invoke(proxy, method, args);
		}
		
		if (ascending == null) {
			ascending = tree.getChild(1).getText().equals("+");
		}
		
		if (isAscending) {
			return ascending;
		}
		
		return (ascending ? '+' : '-') +
			((SimpleSortTerm)proxy).identifier().toString();
	}

	private Boolean ascending = null;
}
