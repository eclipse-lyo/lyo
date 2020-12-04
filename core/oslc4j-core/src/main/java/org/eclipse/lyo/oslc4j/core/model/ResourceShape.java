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
import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Resource Shape Resource Shape", describes = OslcConstants.TYPE_RESOURCE_SHAPE)
public final class ResourceShape extends AbstractResource {
	private final SortedSet<URI> describes= new TreeSet<URI>();
	private final TreeMap<URI, Property> properties = new TreeMap<URI, Property>();

	private String title;

	public ResourceShape() {
		super();
	}

	public ResourceShape(final URI about) {
		super(about);
	}

	public void addDescribeItem(final URI describeItem) {
		this.describes.add(describeItem);
	}

	public void addProperty(final Property property) {
		this.properties.put(property.getPropertyDefinition(), property);
	}
	
	//Bugzilla 392780
	public Property getProperty(URI definition) {
		return properties.get(definition);
	}

	@OslcDescription("Type or types of resource described by this shape")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "describes")
	@OslcReadOnly
	@OslcTitle("Describes")
	public URI[] getDescribes() {
		return describes.toArray(new URI[describes.size()]);
	}

	@OslcDescription("The properties that are allowed or required by this shape")
	@OslcName("property")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "property")
	@OslcRange(OslcConstants.TYPE_PROPERTY)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Properties")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_PROPERTY)
	@OslcValueType(ValueType.LocalResource)
	public Property[] getProperties() {
		return properties.values().toArray(new Property[properties.size()]);
	}

	

	@OslcDescription("Title of the resource shape. SHOULD include only content that is valid and suitable inside an XHTML <div> element")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	@OslcReadOnly
	@OslcTitle("Title")
	@OslcValueType(ValueType.XMLLiteral)
	public String getTitle() {
		return title;
	}

	public void setDescribes(final URI[] describes) {
		this.describes.clear();
		if (describes != null) {
			this.describes.addAll(Arrays.asList(describes));
		}
	}


	public void setProperties(final Property[] properties) {
		this.properties.clear();
		if (properties != null) {
			for(Property prop :properties) {
				this.properties.put(prop.getPropertyDefinition(), prop);
			}
		}
	}

	public void setTitle(final String title) {
		this.title = title;
	}
}
