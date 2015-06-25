/*******************************************************************************
 * Copyright (c) 2014, 2015 IBM Corporation.
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
 *     Romain Barth			- unparseable literal
 *       some attributes in DOORS or CLM are not parseable because of some value
 *       (e.g. date in a datetime attribute or empty value in a long attribute)
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core;

/**
 * Represents properties whose value is not valid for the declared
 * datatype. This enables clients to at least see the raw value and
 * preserve the value when updating the resource with PUT. Strict
 * datatypes must be disabled.
 *
 * @see "AbstractOslcRdfXmlProvider.OSLC4J_STRICT_DATATYPES"
 */

public class UnparseableLiteral
{
	private String rawValue;
	private String datatype;

	public UnparseableLiteral(String rawValue, String datatype)
	{
		this.setDatatype(datatype);
		this.setRawValue(rawValue);
	}

	public String getRawValue()
	{
		return this.rawValue;
	}

	public String getDatatype()
	{
		return this.datatype;
	}

	public void setRawValue(String value)
	{
		this.rawValue = value;
	}

	public void setDatatype(String value)
	{
		this.datatype = value;
	}

	public String toString()
	{
		return "unparseable literal: \""+this.rawValue+"\"^^<"+this.datatype+">";
	}
}
