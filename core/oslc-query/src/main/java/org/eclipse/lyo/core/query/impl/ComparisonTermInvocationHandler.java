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
import java.lang.reflect.Proxy;
import java.util.Map;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.Tree;
import org.eclipse.lyo.core.query.BooleanValue;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.ComparisonTerm.Operator;
import org.eclipse.lyo.core.query.DecimalValue;
import org.eclipse.lyo.core.query.LangedStringValue;
import org.eclipse.lyo.core.query.OslcWhereParser;
import org.eclipse.lyo.core.query.SimpleTerm.Type;
import org.eclipse.lyo.core.query.StringValue;
import org.eclipse.lyo.core.query.TypedValue;
import org.eclipse.lyo.core.query.UriRefValue;
import org.eclipse.lyo.core.query.Value;

/**
 * Proxy implmentation of {@link ComparisonTerm} interface
 */
class ComparisonTermInvocationHandler extends SimpleTermInvocationHandler
{
	public
	ComparisonTermInvocationHandler(
		Tree tree,
		Map<String, String> prefixMap
	)
	{
		super(tree, Type.COMPARISON, prefixMap);
		
		switch (((CommonTree)tree.getChild(1)).getType()) {
		case OslcWhereParser.EQUAL:
			operator = Operator.EQUALS;
			break;
		case OslcWhereParser.NOT_EQUAL:
			operator = Operator.NOT_EQUALS;
			break;
		case OslcWhereParser.LESS:
			operator = Operator.LESS_THAN;
			break;
		case OslcWhereParser.LESS_EQUAL:
			operator = Operator.LESS_EQUALS;
			break;
		case OslcWhereParser.GREATER:
			operator = Operator.GREATER_THAN;
			break;
		default:
		case OslcWhereParser.GREATER_EQUAL:
			operator = Operator.GREATER_EQUALS;
			break;
		}
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
		
		if (methodName.equals("operator")) {
			return operator;
		}
		
		boolean isOperand = methodName.equals("operand");
		
		if (! isOperand &&
			! methodName.equals("toString")) {
			return super.invoke(proxy, method, args);
		}
		
		if (operand == null) {
			
			CommonTree treeOperand = (CommonTree)tree.getChild(2);
			
			operand = createValue(treeOperand, "unspported literal value type",
								  prefixMap);			 
		}
		
		if (isOperand) {
			return operand;
		}
		
		return ((ComparisonTerm)proxy).property().toString() +
			operator.toString() + operand.toString();
	}
	
	static Value
	createValue(
		Tree treeOperand,
		String errorPrefix,
		Map<String, String> prefixMap
	)
	{
		switch (treeOperand.getType()) {
		case OslcWhereParser.IRI_REF:
			return
				(Value)Proxy.newProxyInstance(
						UriRefValue.class.getClassLoader(), 
						new Class<?>[] { UriRefValue.class },
						new UriRefValueInvocationHandler(
							   treeOperand));
		case OslcWhereParser.BOOLEAN:
			return
				(Value)Proxy.newProxyInstance(
						BooleanValue.class.getClassLoader(), 
						new Class<?>[] { BooleanValue.class },
						new BooleanValueInvocationHandler(
							   treeOperand));
		case OslcWhereParser.DECIMAL:
			return
				(Value)Proxy.newProxyInstance(
						DecimalValue.class.getClassLoader(), 
						new Class<?>[] { DecimalValue.class },
						new DecimalValueInvocationHandler(
							   treeOperand));
		case OslcWhereParser.STRING_LITERAL:
			return
				(Value)Proxy.newProxyInstance(
						StringValue.class.getClassLoader(), 
						new Class<?>[] { StringValue.class },
						new StringValueInvocationHandler(
							   treeOperand));
		case OslcWhereParser.TYPED_VALUE:
			return
				(Value)Proxy.newProxyInstance(
						TypedValue.class.getClassLoader(), 
						new Class<?>[] { TypedValue.class },
						new TypedValueInvocationHandler(
							   treeOperand, prefixMap));
		case OslcWhereParser.LANGED_VALUE:
			return
				(Value)Proxy.newProxyInstance(
						LangedStringValue.class.getClassLoader(), 
						new Class<?>[] { LangedStringValue.class },
						new LangedStringValueInvocationHandler(
							   treeOperand));
		default:
			throw new IllegalStateException(
					errorPrefix + ": " +
						treeOperand.getText());
		}		
	}
	
	private final Operator operator;
	private Value operand = null;
}
