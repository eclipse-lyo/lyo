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
import org.eclipse.lyo.core.query.BooleanValue;
import org.eclipse.lyo.core.query.Value.Type;

/**
 * Proxy implementation of {@link BooleanValue} interface
 */
class BooleanValueInvocationHandler extends ValueInvocationHandler
{
	public
	BooleanValueInvocationHandler(Tree tree)
	{
		super(tree, Type.BOOLEAN);
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
			value = Boolean.valueOf(tree.getText());
		}
		
		if (isValue) {
			return value;
		}
		
		return value.toString();
	}
	
	private Boolean value = null;
}
