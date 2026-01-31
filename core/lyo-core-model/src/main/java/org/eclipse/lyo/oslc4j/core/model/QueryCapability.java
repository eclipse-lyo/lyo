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

/**
 * OSLC QueryCapability resource
 */
@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(
    title = "OSLC Query Capability Resource Shape",
    describes = OslcConstants.TYPE_QUERY_CAPABILITY)
public class QueryCapability extends AbstractResource {
  private final SortedSet<URI> resourceTypes = new TreeSet<>();
  private final SortedSet<URI> usages = new TreeSet<>();

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
   * The base URI to use for queries. Queries are invoked via HTTP GET on a query URI formed by appending a key=value pair to the base URI, as described in Query Capabilities section
   *
   * @return queryBase
   */
  @OslcDescription(
      "The base URI to use for queries. Queries are invoked via HTTP GET on a query URI formed by"
          + " appending a key=value pair to the base URI, as described in Query Capabilities"
          + " section")
  @OslcOccurs(Occurs.ExactlyOne)
  @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "queryBase")
  @OslcReadOnly
  @OslcTitle("Query Base")
  public URI getQueryBase() {
    return queryBase;
  }

  /**
   * The Query Capability SHOULD provide a Resource Shape that describes the query base URI
   *
   * @return resourceShape
   */
  @OslcDescription(
      "The Query Capability SHOULD provide a Resource Shape that describes the query base URI")
  @OslcName("resourceShape")
  @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "resourceShape")
  @OslcRange(OslcConstants.TYPE_RESOURCE_SHAPE)
  @OslcReadOnly
  @OslcTitle("Resource Shape")
  @OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_RESOURCE_SHAPE)
  public URI getResourceShape() {
    return resourceShape;
  }

  /**
   * The expected resource type URI that will be returned with this query capability. These would be the URIs found in the result resource's rdf:type property
   *
   * @return resourceTypes
   */
  @OslcDescription(
      "The expected resource type URI that will be returned with this query capability. These would"
          + " be the URIs found in the result resource's rdf:type property")
  @OslcName("resourceType")
  @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "resourceType")
  @OslcReadOnly
  @OslcTitle("Resource Types")
  public URI[] getResourceTypes() {
    return resourceTypes.toArray(new URI[resourceTypes.size()]);
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

  /**
   * An identifier URI for the domain specified usage of this query capability. If a service provides multiple query capabilities, it may designate the primary or default one that should be used with a property value of http://open-services/ns/core#default
   *
   * @return usages
   */
  @OslcDescription(
      "An identifier URI for the domain specified usage of this query capability. If a service"
          + " provides multiple query capabilities, it may designate the primary or default one"
          + " that should be used with a property value of http://open-services/ns/core#default")
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
