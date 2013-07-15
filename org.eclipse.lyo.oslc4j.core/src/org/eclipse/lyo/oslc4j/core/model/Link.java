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
 *     Samuel Padgett - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;

/**
 * Special OSLC link type. Differs from {@link URI} since it can hold a label,
 * expressed in RDF using reification.
 * 
 * @see <a href="http://open-services.net/bin/view/Main/OslcCoreSpecAppendixLinks">OSLC Core Specification 2.0, Appendix C: Guidance on Links & Relationships</a>
 */
public class Link extends AbstractReifiedResource<URI>
{
	private String label;

	public Link()
	{	
	}
	
	public Link(URI resource)
	{
		setValue(resource);
	}
	
	public Link(URI resource, String label)
	{
		setValue(resource);
		this.label = label;
	}

	@OslcName("title")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	/**
	 * Gets the link label.
	 * 
	 * @return the label
	 */
	public String getLabel()
	{
		return label;
	}

	/**
	 * Sets the link label.
	 * 
	 * @param label the label
	 */
	public void setLabel(String label)
	{
		this.label = label;
	}	
}
