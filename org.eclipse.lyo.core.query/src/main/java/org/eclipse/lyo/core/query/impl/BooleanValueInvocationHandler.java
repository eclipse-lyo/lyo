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
