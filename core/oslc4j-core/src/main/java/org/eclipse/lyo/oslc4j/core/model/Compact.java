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

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Compact Resource Shape", describes = OslcConstants.TYPE_COMPACT)
public class Compact extends AbstractResource {
	private URI icon;
	private Preview largePreview;
	private String shortTitle;
	private Preview smallPreview;
	private String title;

	public Compact() {
		super();
	}

	@OslcDescription("URI of an image which may be used in the display of a link to the resource. The image SHOULD be 16x16 pixels in size.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "icon")
	@OslcReadOnly
	@OslcTitle("Icon")
	public URI getIcon() {
		return icon;
	}

	@OslcDescription("URI and sizing properties for an HTML document to be used for a large preview.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "largePreview")
	@OslcRange(OslcConstants.TYPE_PREVIEW)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Large Preview")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_PREVIEW)
	@OslcValueType(ValueType.LocalResource)
	public Preview getLargePreview() {
		return largePreview;
	}

	@OslcDescription("Abbreviated title which may be used in the display of a link to the resource.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "shortTitle")
	@OslcReadOnly
	@OslcTitle("Short Title")
	public String getShortTitle() {
		return shortTitle;
	}

	@OslcDescription("URI and sizing properties for an HTML document to be used for a small preview.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "smallPreview")
	@OslcRange(OslcConstants.TYPE_PREVIEW)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Small Preview")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_PREVIEW)
	@OslcValueType(ValueType.LocalResource)
	public Preview getSmallPreview() {
		return smallPreview;
	}

	@OslcDescription("Title which may be used in the display of a link to the resource.")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	@OslcReadOnly
	@OslcTitle("Title")
	@OslcValueType(ValueType.String)
	public String getTitle() {
		return title;
	}

	public void setIcon(final URI icon) {
		this.icon = icon;
	}

	public void setLargePreview(final Preview largePreview) {
		this.largePreview = largePreview;
	}

	public void setShortTitle(final String shortTitle) {
		this.shortTitle = shortTitle;
	}

	public void setSmallPreview(final Preview smallPreview) {
		this.smallPreview = smallPreview;
	}

	public void setTitle(final String title) {
		this.title = title;
	}
}
