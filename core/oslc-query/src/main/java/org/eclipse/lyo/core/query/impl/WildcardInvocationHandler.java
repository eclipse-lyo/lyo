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

import org.eclipse.lyo.core.query.Property.Type;


/**
 * Proxy implementation of {@link org.eclipse.lyo.core.query.Wildcard Wildcard} interface
 */
public class WildcardInvocationHandler extends PropertyInvocationHandler
{
	public
	WildcardInvocationHandler()
	{
		super(null, Type.IDENTIFIER, null, true);
	}

	/**
	 * @see org.eclipse.lyo.core.query.impl.PropertiesInvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	@Override
	public Object invoke(
		Object proxy,
		Method method,
		Object[] args
	) throws Throwable
	{
		if (! method.getName().equals("toString")) {
			return super.invoke(proxy, method, args);
		}
		
		return "*";
	}
}
