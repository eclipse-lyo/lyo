// Start of user code Copyright
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
// End of user code

package org.eclipse.lyo.oslc.domains.am;

// spotless:off
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.text.SimpleDateFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.Iterator;

import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcMemberProperty;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRdfCollectionType;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.Representation;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

import org.eclipse.lyo.oslc.domains.am.Oslc_amDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.RdfsDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.OsclVocabularyConstants;
import org.eclipse.lyo.oslc.domains.RdfsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.IPerson;

// Start of user code imports
// End of user code
// spotless:on

@OslcNamespace(Oslc_amDomainConstants.LINKTYPE_NAMESPACE)
@OslcName(Oslc_amDomainConstants.LINKTYPE_LOCALNAME)
@OslcResourceShape(title = "LinkType Shape", describes = Oslc_amDomainConstants.LINKTYPE_TYPE)
public interface ILinkType {

  public void addContributor(final Link contributor);

  public void addCreator(final Link creator);

  public void addInstanceShape(final Link instanceShape);

  public void addServiceProvider(final Link serviceProvider);

  @OslcName("contributor")
  @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "contributor")
  @OslcDescription(
      "Contributor or contributors to the resource. It is likely that the target resource will be a"
          + " foaf:Person but that is not necessarily the case.")
  @OslcOccurs(Occurs.ZeroOrMany)
  @OslcValueType(ValueType.Resource)
  @OslcRange({FoafDomainConstants.PERSON_TYPE})
  @OslcReadOnly(false)
  public Set<Link> getContributor();

  @OslcName("created")
  @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "created")
  @OslcDescription("Timestamp of resource creation")
  @OslcOccurs(Occurs.ZeroOrOne)
  @OslcValueType(ValueType.DateTime)
  @OslcReadOnly(false)
  public Date getCreated();

  @OslcName("creator")
  @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "creator")
  @OslcDescription(
      "Creator or creators of the resource. It is likely that the target resource will be a"
          + " foaf:Person but that is not necessarily the case.")
  @OslcOccurs(Occurs.ZeroOrMany)
  @OslcValueType(ValueType.Resource)
  @OslcRange({FoafDomainConstants.PERSON_TYPE})
  @OslcReadOnly(false)
  public Set<Link> getCreator();

  @OslcName("identifier")
  @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "identifier")
  @OslcDescription(
      "A unique identifier for a resource. Typically read-only and assigned by the service provider"
          + " when a resource is created. Not typically intended for end-user display.")
  @OslcOccurs(Occurs.ExactlyOne)
  @OslcValueType(ValueType.String)
  @OslcReadOnly(false)
  public String getIdentifier();

  @OslcName("modified")
  @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "modified")
  @OslcDescription("Timestamp of latest resource modification")
  @OslcOccurs(Occurs.ZeroOrOne)
  @OslcValueType(ValueType.DateTime)
  @OslcReadOnly(false)
  public Date getModified();

  @OslcName("instanceShape")
  @OslcPropertyDefinition(OsclVocabularyConstants.OSLC_CORE_NAMSPACE + "instanceShape")
  @OslcDescription(
      "The URI of a Resource Shape that describes the possible properties, occurrence, value types,"
          + " allowed values and labels. This shape information is useful in displaying the subject"
          + " resource as well as guiding clients in performing modifications. Instance shapes may"
          + " be specific to the authenticated user associated with the request that retrieved the"
          + " resource, the current state of the resource and other factors and thus should not be"
          + " cached.")
  @OslcOccurs(Occurs.ZeroOrMany)
  @OslcValueType(ValueType.Resource)
  @OslcRepresentation(Representation.Reference)
  @OslcRange({OslcDomainConstants.RESOURCESHAPE_TYPE})
  @OslcReadOnly(false)
  public Set<Link> getInstanceShape();

  @OslcName("serviceProvider")
  @OslcPropertyDefinition(OsclVocabularyConstants.OSLC_CORE_NAMSPACE + "serviceProvider")
  @OslcDescription(
      "A link to the resource's OSLC Service Provider. There may be cases when the subject resource"
          + " is available from a service provider that implements multiple domain specifications,"
          + " which could result in multiple values for this property.")
  @OslcOccurs(Occurs.ZeroOrMany)
  @OslcValueType(ValueType.Resource)
  @OslcRepresentation(Representation.Reference)
  @OslcRange({OslcDomainConstants.SERVICEPROVIDER_TYPE})
  @OslcReadOnly(false)
  public Set<Link> getServiceProvider();

  @OslcName("comment")
  @OslcPropertyDefinition(RdfsVocabularyConstants.RDFS_NAMSPACE + "comment")
  @OslcDescription("May be used to provide a human-readable description of a resource.")
  @OslcOccurs(Occurs.ExactlyOne)
  @OslcValueType(ValueType.XMLLiteral)
  @OslcReadOnly(false)
  public String getComment();

  @OslcName("label")
  @OslcPropertyDefinition(RdfsVocabularyConstants.RDFS_NAMSPACE + "label")
  @OslcDescription("May be used to provide a human-readable version of a resource's name.")
  @OslcOccurs(Occurs.ZeroOrOne)
  @OslcValueType(ValueType.XMLLiteral)
  @OslcReadOnly(false)
  public String getLabel();

  public void setContributor(final Set<Link> contributor);

  public void setCreated(final Date created);

  public void setCreator(final Set<Link> creator);

  public void setIdentifier(final String identifier);

  public void setModified(final Date modified);

  public void setInstanceShape(final Set<Link> instanceShape);

  public void setServiceProvider(final Set<Link> serviceProvider);

  public void setComment(final String comment);

  public void setLabel(final String label);
}
