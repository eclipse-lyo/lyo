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
