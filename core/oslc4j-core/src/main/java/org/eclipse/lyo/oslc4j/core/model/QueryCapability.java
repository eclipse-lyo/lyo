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
@OslcResourceShape(title = "OSLC Query Capability Resource Shape", describes = OslcConstants.TYPE_QUERY_CAPABILITY)
public class QueryCapability extends AbstractResource {
	private final SortedSet<URI> resourceTypes = new TreeSet<URI>();
	private final SortedSet<URI> usages = new TreeSet<URI>();

	private String label;
	private URI queryBase;
	private URI resourceShape;
	private String title;

	public QueryCapability() {
		super();
	}

	public QueryCapability(final String title, final URI queryBase) {
		this();

		this.title = title;
		this.queryBase = queryBase;
	}

	public void addResourceType(final URI resourceType) {
		this.resourceTypes.add(resourceType);
	}

	public void addUsage(final URI usage) {
		this.usages.add(usage);
	}

	@OslcDescription("Very short label for use in menu items")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "label")
	@OslcReadOnly
	@OslcTitle("Label")
	public String getLabel() {
		return label;
	}

	@OslcDescription("The base URI to use for queries. Queries are invoked via HTTP GET on a query URI formed by appending a key=value pair to the base URI, as described in Query Capabilities section")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "queryBase")
	@OslcReadOnly
	@OslcTitle("Query Base")
	public URI getQueryBase() {
		return queryBase;
	}

	@OslcDescription("The Query Capability SHOULD provide a Resource Shape that describes the query base URI")
	@OslcName("resourceShape")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "resourceShape")
	@OslcRange(OslcConstants.TYPE_RESOURCE_SHAPE)
	@OslcReadOnly
	@OslcTitle("Resource Shape")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_RESOURCE_SHAPE)
	public URI getResourceShape() {
		return resourceShape;
	}

	@OslcDescription("The expected resource type URI that will be returned with this query capability. These would be the URIs found in the result resource's rdf:type property")
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

	@OslcDescription("An identifier URI for the domain specified usage of this query capability. If a service provides multiple query capabilities, it may designate the primary or default one that should be used with a property value of http://open-services/ns/core#default")
	@OslcName("usage")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "usage")
	@OslcReadOnly
	@OslcTitle("Usages")
	public URI[] getUsages() {
		return usages.toArray(new URI[usages.size()]);
	}

	public void setLabel(final String label) {
		this.label = label;
	}

	public void setQueryBase(final URI queryBase) {
		this.queryBase = queryBase;
	}

	public void setResourceShape(final URI resourceShape) {
		this.resourceShape = resourceShape;
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
