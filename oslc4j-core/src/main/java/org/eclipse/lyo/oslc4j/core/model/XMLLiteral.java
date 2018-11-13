/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
 *	   Samuel Padgett		- support XML Literals in extended properties
 *******************************************************************************/
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
