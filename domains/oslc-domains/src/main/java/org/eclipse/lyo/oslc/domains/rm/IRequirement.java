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

package org.eclipse.lyo.oslc.domains.rm;

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

import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;

import org.eclipse.lyo.oslc.domains.Oslc_rmVocabularyConstants;
import org.eclipse.lyo.oslc.domains.IPerson;
import org.eclipse.lyo.oslc.domains.IPerson;

// Start of user code imports
import org.eclipse.lyo.oslc.domains.Oslc_rmVocabularyConstants;
// End of user code

@OslcNamespace(Oslc_rmDomainConstants.REQUIREMENT_NAMESPACE)
@OslcName(Oslc_rmDomainConstants.REQUIREMENT_LOCALNAME)
@OslcResourceShape(title = "Requirement Resource Shape", describes = Oslc_rmDomainConstants.REQUIREMENT_TYPE)
public interface IRequirement
{

    public void addSubject(final String subject );
    public void addCreator(final Link creator );
    public void addContributor(final Link contributor );
    public void addServiceProvider(final Link serviceProvider );
    public void addInstanceShape(final Link instanceShape );
    public void addElaboratedBy(final Link elaboratedBy );
    public void addElaborates(final Link elaborates );
    public void addSpecifiedBy(final Link specifiedBy );
    public void addSpecifies(final Link specifies );
    public void addAffectedBy(final Link affectedBy );
    public void addTrackedBy(final Link trackedBy );
    public void addImplementedBy(final Link implementedBy );
    public void addValidatedBy(final Link validatedBy );
    public void addSatisfiedBy(final Link satisfiedBy );
    public void addSatisfies(final Link satisfies );
    public void addDecomposedBy(final Link decomposedBy );
    public void addDecomposes(final Link decomposes );
    public void addConstrainedBy(final Link constrainedBy );
    public void addConstrains(final Link constrains );

    @OslcName("title")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription("Title of the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle();

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

    @OslcName("shortTitle")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "shortTitle")
    @OslcDescription("Shorter form of dcterms:title for the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getShortTitle();

    @OslcName("subject")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "subject")
    @OslcDescription("Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes an additional tag for the resource.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    @OslcTitle("")
    public Set<String> getSubject();

    @OslcName("creator")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "creator")
    @OslcDescription("Creator or creators of the resource. It is likely that the target resource will be a foaf:Person but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getCreator();

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

    @OslcName("modified")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "modified")
    @OslcDescription("Timestamp of latest resource modification")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getModified();

    @OslcName("serviceProvider")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "serviceProvider")
    @OslcDescription("A link to the resource's OSLC Service Provider. There may be cases when the subject resource is available from a service provider that implements multiple domain specifications, which could result in multiple values for this property.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getServiceProvider();

    @OslcName("instanceShape")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "instanceShape")
    @OslcDescription("The URI of a Resource Shape that describes the possible properties, occurrence, value types, allowed values and labels. This shape information is useful in displaying the subject resource as well as guiding clients in performing modifications. Instance shapes may be specific to the authenticated user associated with the request that retrieved the resource, the current state of the resource and other factors and thus should not be cached.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getInstanceShape();

    @OslcName("elaboratedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "elaboratedBy")
    @OslcDescription("The subject is elaborated by the object. For example, a user requirement is elaborated by use case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getElaboratedBy();

    @OslcName("elaborates")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "elaborates")
    @OslcDescription("The object is elaborated by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getElaborates();

    @OslcName("specifiedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "specifiedBy")
    @OslcDescription("The subject is specified by the object. For example, a requirement is elaborated by a model element .")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSpecifiedBy();

    @OslcName("specifies")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "specifies")
    @OslcDescription("The object is specified by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSpecifies();

    @OslcName("affectedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "affectedBy")
    @OslcDescription("Requirement is affected by a resource, such as a defect or issue.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getAffectedBy();

    @OslcName("trackedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "trackedBy")
    @OslcDescription("Resource, such as a change request, which tracks this requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getTrackedBy();

    @OslcName("implementedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "implementedBy")
    @OslcDescription("Resource, such as a change request, which implements this requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getImplementedBy();

    @OslcName("validatedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "validatedBy")
    @OslcDescription("Resource, such as a test case, which validates this requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getValidatedBy();

    @OslcName("satisfiedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "satisfiedBy")
    @OslcDescription("The subject is satisfied by the object. For example, a user requirement is satisfied by a system requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSatisfiedBy();

    @OslcName("satisfies")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "satisfies")
    @OslcDescription("The object is satisfied by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getSatisfies();

    @OslcName("decomposedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "decomposedBy")
    @OslcDescription("The subject is decomposed by the object. For example, a system requirement is decomposed into a collection of system requirements.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getDecomposedBy();

    @OslcName("decomposes")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "decomposes")
    @OslcDescription("The object is decomposed by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getDecomposes();

    @OslcName("constrainedBy")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "constrainedBy")
    @OslcDescription("The subject is constrained by the object. For example, a functional requirement is constrained by a safety requirement.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getConstrainedBy();

    @OslcName("constrains")
    @OslcPropertyDefinition(Oslc_rmVocabularyConstants.REQUIREMENTS_MANAGEMENT_VOCABULARY_NAMSPACE + "constrains")
    @OslcDescription("The object is constrained by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getConstrains();


    public void setTitle(final String title );
    public void setDescription(final String description );
    public void setIdentifier(final String identifier );
    public void setShortTitle(final String shortTitle );
    public void setSubject(final Set<String> subject );
    public void setCreator(final Set<Link> creator );
    public void setContributor(final Set<Link> contributor );
    public void setCreated(final Date created );
    public void setModified(final Date modified );
    public void setServiceProvider(final Set<Link> serviceProvider );
    public void setInstanceShape(final Set<Link> instanceShape );
    public void setElaboratedBy(final Set<Link> elaboratedBy );
    public void setElaborates(final Set<Link> elaborates );
    public void setSpecifiedBy(final Set<Link> specifiedBy );
    public void setSpecifies(final Set<Link> specifies );
    public void setAffectedBy(final Set<Link> affectedBy );
    public void setTrackedBy(final Set<Link> trackedBy );
    public void setImplementedBy(final Set<Link> implementedBy );
    public void setValidatedBy(final Set<Link> validatedBy );
    public void setSatisfiedBy(final Set<Link> satisfiedBy );
    public void setSatisfies(final Set<Link> satisfies );
    public void setDecomposedBy(final Set<Link> decomposedBy );
    public void setDecomposes(final Set<Link> decomposes );
    public void setConstrainedBy(final Set<Link> constrainedBy );
    public void setConstrains(final Set<Link> constrains );
}

