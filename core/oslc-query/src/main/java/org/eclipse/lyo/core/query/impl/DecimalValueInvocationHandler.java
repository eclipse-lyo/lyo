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

import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.DecimalValue;
import org.eclipse.lyo.core.query.Value.Type;

/**
 * Proxy implementation of {@link DecimalValue} interface
 */
class DecimalValueInvocationHandler extends ValueInvocationHandler
{
	public
	DecimalValueInvocationHandler(Tree tree)
	{
		super(tree, Type.DECIMAL);
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
		boolean isValue = methodName.equals("value");
		
		if (! isValue &&
			! methodName.equals("toString")) {
			return super.invoke(proxy, method, args);
		}
		
		if (value == null) {
			value = tree.getText();
		}
		
		return value;
	}
	
	private String value = null;
}
