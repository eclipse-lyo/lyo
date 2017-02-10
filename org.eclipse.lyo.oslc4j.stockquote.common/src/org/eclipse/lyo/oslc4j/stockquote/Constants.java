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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.stockquote;

public interface Constants
{
	public static String STOCK_QUOTE_DOMAIN			  = "http://yourcompany.com/ns/stockquote#";
	public static String STOCK_QUOTE_NAMESPACE		  = "http://yourcompany.com/ns/stockquote#";
	public static String STOCK_QUOTE_NAMESPACE_PREFIX = "stockquote";

	public static String TYPE_STOCK_QUOTE = STOCK_QUOTE_NAMESPACE + "StockQuote";

	public static String PATH_STOCK_QUOTE = "stockQuote";
}
