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

package org.eclipse.lyo.core.query;

/**
 * Simple comparison term from oslc.where clause
 */
public interface ComparisonTerm extends SimpleTerm
{
	enum Operator
	{
		EQUALS,
		NOT_EQUALS,
		LESS_THAN,
		GREATER_THAN,
		LESS_EQUALS,
		GREATER_EQUALS;
		
		public String
		toString()
		{
			switch (this)
			{
			case EQUALS:
				return "=";
			case NOT_EQUALS:
				return "!=";
			case LESS_THAN:
				return "<";
			case GREATER_THAN:
				return ">";
			case LESS_EQUALS:
				return "<=";
			default:
			case GREATER_EQUALS:
				return ">=";
			}
		}
	};
	
	Operator operator();
	
	Value operand();
}
