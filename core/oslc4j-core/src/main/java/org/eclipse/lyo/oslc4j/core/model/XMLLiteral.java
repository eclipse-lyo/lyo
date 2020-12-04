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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.oslc4j.core.model;

/**
 * A special value type that can be added to an extended properties map of an
 * {@link IExtendedResource} to indicate the value is an XMLLiteral rather than
 * a simple string.
 * 
 * @see <a href="http://www.w3.org/TR/rdf-concepts/#section-XMLLiteral">XML Content within an RDF Graph</a>
 */
public class XMLLiteral
{
	private String value;
	
	/**
	 * Creates an XMLLiteral.
	 * 
	 * @param value a plain string for the literal (with markup unescaped).
	 */
	public XMLLiteral(String value)
	{
		this.value = value;
	}
	
	/**
	 * Gets the string value of the literal.
	 * 
	 * @return the value
	 */
	public String getValue()
	{
		return value;
	}
	
	@Override
	public String toString()
	{
		return value;
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
		{
			return true;
		}

		if (obj == null)
		{
			return false;
		}

		if (getClass() != obj.getClass())
		{
			return false;
		}
		
		XMLLiteral other = (XMLLiteral) obj;
		if (value == null)
		{
			if (other.value != null)
			{
				return false;
			}
		}
		else if (!value.equals(other.value))
		{
			return false;
		}
	
		return true;
	}
}
