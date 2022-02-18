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

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.OslcOrderByParser;
import org.eclipse.lyo.core.query.ScopedSortTerm;
import org.eclipse.lyo.core.query.SimpleSortTerm;
import org.eclipse.lyo.core.query.SortTerm;

/**
 * Proxy implementation of {@link org.eclipse.lyo.core.query.SortTerms SortTerms}
 */
public class SortTermsInvocationHandler implements InvocationHandler
{
	public
	SortTermsInvocationHandler(
		Tree tree,
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

			children = new ArrayList<>(tree.getChildCount());

			for (int index = 0; index < tree.getChildCount(); index++) {

				Tree child = tree.getChild(index);

				Object simpleTerm;

				switch(child.getType()) {
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
					throw new IllegalStateException("unimplemented type of sort term: " + child.getText());
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

	private final Tree tree;
	private final Map<String, String> prefixMap;
	private List<SortTerm> children = null;
}
