/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *	   Samuel Padgett		- remove final from class
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Publisher Resource Shape", describes = OslcConstants.TYPE_PUBLISHER)
public class Publisher extends AbstractResource {
	private URI	   icon;
	private String identifier;
	private String label;
	private String title;

	public Publisher() {
		super();
	}

	public Publisher(final String title, final String identifier) {
		this();

		this.title = title;
		this.identifier = identifier;
	}

	@OslcDescription("URL to an icon file that represents the provider. This icon should be a favicon format and 16x16 pixels in size")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "icon")
	@OslcReadOnly
	@OslcTitle("Icon")
	public URI getIcon() {
		return icon;
	}

	@OslcDescription("A URN that uniquely identifies the implementation")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "identifier")
	@OslcReadOnly // TODO - Marked as unspecified in the spec, but is this correct?
	@OslcTitle("Identifier")
	public String getIdentifier() {
		return identifier;
	}

	@OslcDescription("Very short label for use in menu items")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "label")
	@OslcReadOnly
	@OslcTitle("Label")
	public String getLabel() {
		return label;
	}

	@OslcDescription("Title string that could be used for display")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	@OslcReadOnly
	@OslcTitle("Title")
	@OslcValueType(ValueType.XMLLiteral)
	public String getTitle() {
		return title;
	}

	public void setIcon(final URI icon) {
		this.icon = icon;
	}

	public void setIdentifier(final String identifier) {
		this.identifier = identifier;
	}

	public void setLabel(final String label) {
		this.label = label;
	}

	public void setTitle(final String title) {
		this.title = title;
	}
}
