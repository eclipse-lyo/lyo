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
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Creation Factory Resource Shape", describes = OslcConstants.TYPE_CREATION_FACTORY)
public class CreationFactory extends AbstractResource {
	private final SortedSet<URI> resourceShapes = new TreeSet<URI>();
	private final SortedSet<URI> resourceTypes = new TreeSet<URI>();
	private final SortedSet<URI> usages = new TreeSet<URI>();

	private URI creation;
	private String label;
	private String title;

	public CreationFactory() {
		super();
	}

	public CreationFactory(final String title, final URI creation) {
		this();

		this.title = title;
		this.creation = creation;
	}

	public void addResourceShape(final URI resourceShape) {
		this.resourceShapes.add(resourceShape);
	}

	public void addResourceType(final URI resourceType) {
		this.resourceTypes.add(resourceType);
	}

	public void addUsage(final URI usage) {
		this.usages.add(usage);
	}

	@OslcDescription("To create a new resource via the factory, post it to this URI")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "creation")
	@OslcReadOnly
	@OslcTitle("Creation")
	public URI getCreation() {
		return creation;
	}

	@OslcDescription("Very short label for use in menu items")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "label")
	@OslcReadOnly
	@OslcTitle("Label")
	public String getLabel() {
		return label;
	}

	@OslcDescription("A creation factory may provide resource shapes that describe shapes of resources that may be created")
	@OslcName("resourceShape")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "resourceShape")
	@OslcRange(OslcConstants.TYPE_RESOURCE_SHAPE)
	@OslcReadOnly
	@OslcTitle("Resource Shapes")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_RESOURCE_SHAPE)
	public URI[] getResourceShapes() {
		return resourceShapes.toArray(new URI[resourceShapes.size()]);
	}

	@OslcDescription("The expected resource type URI of the resource that will be created using this creation factory. These would be the URIs found in the result resource's rdf:type property")
	@OslcName("resourceType")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "resourceType")
	@OslcReadOnly
	@OslcTitle("Resource Types")
	public URI[] getResourceTypes() {
		return resourceTypes.toArray(new URI[resourceTypes.size()]);
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

	@OslcDescription("An identifier URI for the domain specified usage of this creation factory. If a service provides multiple creation factories, it may designate the primary or default one that should be used with a property value of http://open-services.net/ns/core#default")
	@OslcName("usage")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "usage")
	@OslcReadOnly
	@OslcTitle("Usages")
	public URI[] getUsages() {
		return usages.toArray(new URI[usages.size()]);
	}

	public void setCreation(final URI creation) {
		this.creation = creation;
	}

	public void setLabel(final String label) {
		this.label = label;
	}

	public void setResourceShapes(final URI[] resourceShapes) {
		this.resourceShapes.clear();
		if (resourceShapes != null) {
			this.resourceShapes.addAll(Arrays.asList(resourceShapes));
		}
	}

	public void setResourceTypes(final URI[] resourceTypes) {
		this.resourceTypes.clear();
		if (resourceTypes != null) {
			this.resourceTypes.addAll(Arrays.asList(resourceTypes));
		}
	}

	public void setTitle(final String title) {
		this.title = title;
	}

	public void setUsages(final URI[] usages) {
		this.usages.clear();
		if (usages != null) {
			this.usages.addAll(Arrays.asList(usages));
		}
	}
}
