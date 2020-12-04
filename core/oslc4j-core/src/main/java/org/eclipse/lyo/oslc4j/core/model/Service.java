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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
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
@OslcResourceShape(title = "OSLC Service Resource Shape", describes = OslcConstants.TYPE_SERVICE)
public class Service extends AbstractResource {
	private final List<Dialog> creationDialogs = new ArrayList<Dialog>();
	private final List<CreationFactory> creationFactories = new ArrayList<CreationFactory>();
	private final List<QueryCapability> queryCapabilities = new ArrayList<QueryCapability>();
	private final List<Dialog> selectionDialogs = new ArrayList<Dialog>();
	private final List<URI> usages = new ArrayList<URI>();

	private URI domain;

	public Service() {
		super();
	}

	public Service(final URI domain) {
		this();

		this.domain = domain;
	}

	public void addCreationDialog(final Dialog dialog) {
		this.creationDialogs.add(dialog);
	}

	public void addCreationFactory(final CreationFactory creationFactory) {
		this.creationFactories.add(creationFactory);
	}

	public void addQueryCapability(final QueryCapability queryCapability) {
		this.queryCapabilities.add(queryCapability);
	}

	public void addSelectionDialog(final Dialog dialog) {
		this.selectionDialogs.add(dialog);
	}

	@OslcDescription("Enables clients to create a resource via UI")
	@OslcName("creationDialog")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "creationDialog")
	@OslcRange(OslcConstants.TYPE_DIALOG)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Creation Dialogs")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_DIALOG)
	@OslcValueType(ValueType.LocalResource)
	public Dialog[] getCreationDialogs() {
		return creationDialogs.toArray(new Dialog[creationDialogs.size()]);
	}

	@OslcDescription("Enables clients to create new resources")
	@OslcName("creationFactory")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "creationFactory")
	@OslcRange(OslcConstants.TYPE_CREATION_FACTORY)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Creation Factories")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_CREATION_FACTORY)
	@OslcValueType(ValueType.LocalResource)
	public CreationFactory[] getCreationFactories() {
		return creationFactories.toArray(new CreationFactory[creationFactories.size()]);
	}

	@OslcDescription("Namespace URI of the OSLC domain specification that is implemented by this service")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "domain")
	@OslcReadOnly
	@OslcTitle("Domain")
	public URI getDomain() {
		return domain;
	}

	@OslcDescription("Enables clients query across a collection of resources")
	@OslcName("queryCapability")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "queryCapability")
	@OslcRange(OslcConstants.TYPE_QUERY_CAPABILITY)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Query Capabilities")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_QUERY_CAPABILITY)
	@OslcValueType(ValueType.LocalResource)
	public QueryCapability[] getQueryCapabilities() {
		return queryCapabilities.toArray(new QueryCapability[queryCapabilities.size()]);
	}

	@OslcDescription("Enables clients to select a resource via UI")
	@OslcName("selectionDialog")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "selectionDialog")
	@OslcRange(OslcConstants.TYPE_DIALOG)
	@OslcReadOnly
	@OslcRepresentation(Representation.Inline)
	@OslcTitle("Selection Dialogs")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_DIALOG)
	@OslcValueType(ValueType.LocalResource)
	public Dialog[] getSelectionDialogs() {
		return selectionDialogs.toArray(new Dialog[selectionDialogs.size()]);
	}

	@OslcDescription("An identifier URI for the domain specified usage of this service")
	@OslcName("usage")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "usage")
	@OslcReadOnly
	@OslcRepresentation(Representation.Reference)
	@OslcTitle("Usages")
	@OslcValueType(ValueType.Resource)
	@OslcOccurs(Occurs.ZeroOrMany)
	public URI[] getUsages() {
		return usages.toArray(new URI[usages.size()]);
	}

	public void setCreationDialogs(final Dialog[] creationDialogs) {
		this.creationDialogs.clear();
		if (creationDialogs != null) {
			this.creationDialogs.addAll(Arrays.asList(creationDialogs));
		}
	}

	public void setCreationFactories(final CreationFactory[] creationFactories) {
		this.creationFactories.clear();
		if (creationFactories != null) {
			this.creationFactories.addAll(Arrays.asList(creationFactories));
		}
	}

	public void setDomain(final URI domain) {
		this.domain = domain;
	}

	public void setQueryCapabilities(final QueryCapability[] queryCapabilities) {
		this.queryCapabilities.clear();
		if (queryCapabilities != null) {
			this.queryCapabilities.addAll(Arrays.asList(queryCapabilities));
		}
	}

	public void setSelectionDialogs(final Dialog[] selectionDialogs) {
		this.selectionDialogs.clear();
		if (selectionDialogs != null) {
			this.selectionDialogs.addAll(Arrays.asList(selectionDialogs));
		}
	}

	public void setUsages(final URI[] usages) {
		this.usages.clear();
		if (usages != null) {
			this.usages.addAll(Arrays.asList(usages));
		}
	}
}
