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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
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
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Dialog Resource Shape", describes = OslcConstants.TYPE_DIALOG)
public final class Dialog extends AbstractResource {
    private final SortedSet<URI> resourceTypes = new TreeSet<URI>();
    private final SortedSet<URI> usages = new TreeSet<URI>();

    private URI dialog;
	private String hintHeight;
	private String hintWidth;
	private String label;
	private String title;

	public Dialog() {
	    super();
	}

	public Dialog(final String title, final URI dialog) {
		this();

		this.title = title;
		this.dialog = dialog;
	}

    public void addResourceType(final URI resourceType) {
        this.resourceTypes.add(resourceType);
    }

    public void addUsage(final URI usage) {
        this.usages.add(usage);
    }

	@OslcDescription("The URI of the dialog")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "dialog")
	@OslcReadOnly
    @OslcTitle("Dialog")
	public URI getDialog() {
	    return dialog;
	}

	@OslcDescription("Values MUST be expressed in relative length units as defined in the W3C Cascading Style Sheets Specification (CSS 2.1) Em and ex units are interpreted relative to the default system font (at 100% size)")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "hintHeight")
	@OslcReadOnly
    @OslcTitle("Hint Height")
	public String getHintHeight() {
		return hintHeight;
	}

	@OslcDescription("Values MUST be expressed in relative length units as defined in the W3C Cascading Style Sheets Specification (CSS 2.1) Em and ex units are interpreted relative to the default system font (at 100% size)")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "hintWidth")
	@OslcReadOnly
    @OslcTitle("Hint Width")
	public String getHintWidth() {
		return hintWidth;
	}

	@OslcDescription("Very short label for use in menu items")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "label")
	@OslcReadOnly
    @OslcTitle("Label")
	public String getLabel() {
		return label;
	}

	@OslcDescription("The expected resource type URI for the resources that will be returned when using this dialog. These would be the URIs found in the result resource's rdf:type property")
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

	@OslcDescription("An identifier URI for the domain specified usage of this dialog. If a service provides multiple selection or creation dialogs, it may designate the primary or default one that should be used with a property value of http://open-services/ns/core#default")
	@OslcName("usage")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "usage")
	@OslcReadOnly
    @OslcTitle("Usages")
	public URI[] getUsages() {
	    return usages.toArray(new URI[usages.size()]);
	}

	public void setDialog(final URI dialog) {
	    this.dialog = dialog;
	}

	public void setHintHeight(final String hintHeight) {
		this.hintHeight = hintHeight;
	}

	public void setHintWidth(final String hintWidth) {
		this.hintWidth = hintWidth;
	}

	public void setLabel(final String label) {
		this.label = label;
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
