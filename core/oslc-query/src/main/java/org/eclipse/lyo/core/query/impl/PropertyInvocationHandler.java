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
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.PName;
import org.eclipse.lyo.core.query.Property;
import org.eclipse.lyo.core.query.Property.Type;

/**
 * Proxy implementation of {@link Property} interface
 */
class PropertyInvocationHandler implements InvocationHandler
{
	public
	PropertyInvocationHandler(
		CommonTree tree,
		Type type,
		Map<String, String> prefixMap,
		boolean isWildcard
	)
	{
		this.tree = tree;
		this.type = type;
		this.prefixMap = prefixMap;
		this.isWildcard = isWildcard;
	}
	
	/**
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
	@Override
	public Object invoke(
		Object proxy,
		Method method,
		Object[] args
	) throws Throwable
	{
		String methodName = method.getName();

		if (methodName.equals("type")) {
			return type;
		}
		
		if (methodName.equals("isWildcard")) {
			return isWildcard;
		}
		
		boolean isIdentifier = methodName.equals("identifier");
		
		if (isIdentifier && isWildcard) {
			throw new IllegalStateException("wildcard has no identifier");
		}
		
		if (isIdentifier && identifier != null) {
			return identifier;
		}
		
		String rawIdentifier = tree.getText();
		
		identifier = new PName();
		
		int colon = rawIdentifier.indexOf(':');
		
		if (colon < 0) {
			identifier.local = rawIdentifier;
		} else { 
			if (colon > 0) {
				identifier.prefix = rawIdentifier.substring(0, colon);
				identifier.namespace = prefixMap.get(identifier.prefix);
			}
			identifier.local = rawIdentifier.substring(colon + 1);
		}
		
		if (isIdentifier) {
			return identifier;
		}
		
		return identifier.toString();
	}
	
	private final CommonTree tree;
	private final Type type;
	protected final Map<String, String> prefixMap;
	private final Boolean isWildcard;
	private PName identifier = null;
}
