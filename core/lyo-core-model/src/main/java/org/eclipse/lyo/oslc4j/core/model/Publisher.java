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
/**
 * OSLC Publisher resource
 */
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

    /**
     * URL to an icon file that represents the provider. This icon should be a favicon format and 16x16 pixels in size
     *
     * @return icon
     */
	@OslcDescription("URL to an icon file that represents the provider. This icon should be a favicon format and 16x16 pixels in size")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "icon")
	@OslcReadOnly
	@OslcTitle("Icon")
	public URI getIcon() {
		return icon;
	}

    /**
     * A URN that uniquely identifies the implementation
     *
     * @return identifier
     */
	@OslcDescription("A URN that uniquely identifies the implementation")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "identifier")
	@OslcReadOnly // TODO - Marked as unspecified in the spec, but is this correct?
	@OslcTitle("Identifier")
	public String getIdentifier() {
		return identifier;
	}

    /**
     * Very short label for use in menu items
     *
     * @return label
     */
	@OslcDescription("Very short label for use in menu items")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "label")
	@OslcReadOnly
	@OslcTitle("Label")
	public String getLabel() {
		return label;
	}

    /**
     * Title string that could be used for display
     *
     * @return title
     */
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