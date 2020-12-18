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

package org.eclipse.lyo.oslc.domains.config;

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

import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;
import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.ProvDomainConstants;
import org.eclipse.lyo.oslc.domains.RdfDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.RdfVocabularyConstants;
import org.eclipse.lyo.oslc.domains.IPerson;
// Start of user code imports
// End of user code

@OslcNamespace(Oslc_configDomainConstants.VERSIONRESOURCE_NAMESPACE)
@OslcName(Oslc_configDomainConstants.VERSIONRESOURCE_LOCALNAME)
@OslcResourceShape(title = "VersionResource Resource Shape", describes = Oslc_configDomainConstants.VERSIONRESOURCE_TYPE)
public interface IVersionResource
{

    public void addContributor(final Link contributor );
    public void addCreator(final Link creator );
    public void addSubject(final String subject );
    public void addCommitter(final Link committer );
    public void addComponent(final String component );
    public void addInstanceShape(final Link instanceShape );
    public void addServiceProvider(final Link serviceProvider );
    public void addType(final Link type );
    public void addWasDerivedFrom(final Link wasDerivedFrom );
    public void addWasRevisionOf(final Link wasRevisionOf );

    @OslcName("contributor")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "contributor")
    @OslcDescription("Contributor or contributors to the resource. It is likely that the target resource will be a foaf:Person but that is not necessarily the case.")
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
    @OslcDescription("Creator or creators of the resource. It is likely that the target resource will be a foaf:Person but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getCreator();

    @OslcName("description")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "description")
    @OslcDescription("Descriptive text about resource represented as rich text in XHTML content. SHOULD include only content that is valid and suitable inside an XHTML <div> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getDescription();

    @OslcName("identifier")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "identifier")
    @OslcDescription("A unique identifier for a resource. Typically read-only and assigned by the service provider when a resource is created. Not typically intended for end-user display.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getIdentifier();

    @OslcName("isVersionOf")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "isVersionOf")
    @OslcDescription("The concept resource of which this resource is a version. The subject of this property must be the version resource URI.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Link getIsVersionOf();

    @OslcName("modified")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "modified")
    @OslcDescription("Timestamp of latest resource modification")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getModified();

    @OslcName("subject")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "subject")
    @OslcDescription("Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes an additional tag for the resource.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    @OslcTitle("")
    public Set<String> getSubject();

    @OslcName("title")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription("Title of the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle();

    @OslcName("committed")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "committed")
    @OslcDescription("Date and time this version resource was checked in. Absent for mutable (checked out) versions.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCommitted();

    @OslcName("committer")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "committer")
    @OslcDescription("The entity that checked in this version.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(true)
    public Set<Link> getCommitter();

    @OslcName("component")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "component")
    @OslcDescription("The component to which this version belongs. Configuration Management provider should indicate the owning component for each version resource using either this property, or using the membership relationship from the component LDPC.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public Set<String> getComponent();

    @OslcName("versionId")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "versionId")
    @OslcDescription("A short human-readable identifier for the version of a resource. All versioned resources should have this property; where the property is present, this identifier must be unique amongst all currently existing versions of the same concept resource. The subject of this property should be the concept resource URI.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getVersionId();

    @OslcName("instanceShape")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "instanceShape")
    @OslcDescription("The URI of a Resource Shape that describes the possible properties, occurrence, value types, allowed values and labels. This shape information is useful in displaying the subject resource as well as guiding clients in performing modifications. Instance shapes may be specific to the authenticated user associated with the request that retrieved the resource, the current state of the resource and other factors and thus should not be cached.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getInstanceShape();

    @OslcName("modifiedBy")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "modifiedBy")
    @OslcDescription("The URI of a resource describing the entity that most recently modified the subject resource. The link target is usually a foaf:Person or foaf:Agent, but could be any type. This is modeled after dcterms:creator, but Dublin Core currently has no equivalent property.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(false)
    public Link getModifiedBy();

    @OslcName("serviceProvider")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "serviceProvider")
    @OslcDescription("A link to the resource's OSLC Service Provider. There may be cases when the subject resource is available from a service provider that implements multiple domain specifications, which could result in multiple values for this property.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getServiceProvider();

    @OslcName("shortId")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "shortId")
    @OslcDescription("Shorter form of dcterms:identifier for the resource, such as a number.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getShortId();

    @OslcName("shortTitle")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "shortTitle")
    @OslcDescription("Shorter form of dcterms:title for the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getShortTitle();

    @OslcName("type")
    @OslcPropertyDefinition(RdfVocabularyConstants.RDF_NAMSPACE + "type")
    @OslcDescription("The resource type URIs")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(false)
    public Set<Link> getType();

    @OslcName("wasDerivedFrom")
    @OslcPropertyDefinition(ProvDomainConstants.PROVENANCE_NAMSPACE + "wasDerivedFrom")
    @OslcDescription("A resource from which this version was derived. This is likely to reference a different concept resource; use of prov:wasRevisionOf is recommended to indicate an earlier version of the same concept resource. The subject of each instance of this property must be the concept resource URI; the object can be a version resource URI, or a concept resource URI (possibly for a non-versioned resource).")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getWasDerivedFrom();

    @OslcName("wasRevisionOf")
    @OslcPropertyDefinition(ProvDomainConstants.PROVENANCE_NAMSPACE + "wasRevisionOf")
    @OslcDescription("A resource from which this version was derived. This is likely to reference an earlier version of the same concept resource; use of prov:wasDerivedFrom is recommended to indicate an earlier version of a different concept resource. The subject of each instance of this property must be the concept resource URI; the object is likely to be a version resource URI.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getWasRevisionOf();


    public void setContributor(final Set<Link> contributor );
    public void setCreated(final Date created );
    public void setCreator(final Set<Link> creator );
    public void setDescription(final String description );
    public void setIdentifier(final String identifier );
    public void setIsVersionOf(final Link isVersionOf );
    public void setModified(final Date modified );
    public void setSubject(final Set<String> subject );
    public void setTitle(final String title );
    public void setCommitted(final Date committed );
    public void setCommitter(final Set<Link> committer );
    public void setComponent(final Set<String> component );
    public void setVersionId(final String versionId );
    public void setInstanceShape(final Set<Link> instanceShape );
    public void setModifiedBy(final Link modifiedBy );
    public void setServiceProvider(final Set<Link> serviceProvider );
    public void setShortId(final String shortId );
    public void setShortTitle(final String shortTitle );
    public void setType(final Set<Link> type );
    public void setWasDerivedFrom(final Set<Link> wasDerivedFrom );
    public void setWasRevisionOf(final Set<Link> wasRevisionOf );
}

