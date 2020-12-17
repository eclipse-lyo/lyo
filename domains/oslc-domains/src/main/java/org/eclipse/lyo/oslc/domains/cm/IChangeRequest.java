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

package org.eclipse.lyo.oslc.domains.cm;

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

import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;
import org.eclipse.lyo.oslc.domains.cm.Oslc_cmDomainConstants;
import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.rm.Oslc_rmDomainConstants;
import org.eclipse.lyo.oslc.domains.Oslc_cmVocabularyConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.IAgent;
import org.eclipse.lyo.oslc.domains.cm.IChangeRequest;
import org.eclipse.lyo.oslc.domains.config.IChangeSet;
import org.eclipse.lyo.oslc.domains.cm.IDefect;
import org.eclipse.lyo.oslc4j.core.model.IDiscussion;
import org.eclipse.lyo.oslc.domains.IPerson;
import org.eclipse.lyo.oslc.domains.cm.IPriority;
import org.eclipse.lyo.oslc.domains.rm.IRequirement;
import org.eclipse.lyo.oslc.domains.cm.IState;
// Start of user code imports
import org.eclipse.lyo.oslc.domains.Oslc_cmVocabularyConstants;
// End of user code

@OslcNamespace(Oslc_cmDomainConstants.CHANGEREQUEST_NAMESPACE)
@OslcName(Oslc_cmDomainConstants.CHANGEREQUEST_LOCALNAME)
@OslcResourceShape(title = "ChangeRequest Resource Shape", describes = Oslc_cmDomainConstants.CHANGEREQUEST_TYPE)
public interface IChangeRequest
{

    public void addSubject(final String subject );
    public void addCreator(final Link creator );
    public void addContributor(final Link contributor );
    public void addServiceProvider(final Link serviceProvider );
    public void addInstanceShape(final Link instanceShape );
    public void addRelatedChangeRequest(final Link relatedChangeRequest );
    public void addAffectsPlanItem(final Link affectsPlanItem );
    public void addAffectedByDefect(final Link affectedByDefect );
    public void addTracksRequirement(final Link tracksRequirement );
    public void addImplementsRequirement(final Link implementsRequirement );
    public void addAffectsRequirement(final Link affectsRequirement );
    public void addTracksChangeSet(final Link tracksChangeSet );
    public void addParent(final Link parent );
    public void addPriority(final Link priority );
    public void addAuthorizer(final Link authorizer );

    @OslcName("shortTitle")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "shortTitle")
    @OslcDescription("Shorter form of dcterms:title for the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getShortTitle();

    @OslcName("description")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "description")
    @OslcDescription("Descriptive text about resource represented as rich text in XHTML content. SHOULD include only content that is valid and suitable inside an XHTML <div> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getDescription();

    @OslcName("title")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription("Title of the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle();

    @OslcName("identifier")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "identifier")
    @OslcDescription("A unique identifier for a resource. Typically read-only and assigned by the service provider when a resource is created. Not typically intended for end-user display.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getIdentifier();

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

    @OslcName("discussedBy")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "discussedBy")
    @OslcDescription("A series of notes and comments about this resource.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({OslcDomainConstants.DISCUSSION_TYPE})
    @OslcReadOnly(false)
    public Link getDiscussedBy();

    @OslcName("closeDate")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "closeDate")
    @OslcDescription("The date at which no further activity or work is intended to be conducted.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCloseDate();

    @OslcName("status")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "status")
    @OslcDescription("Used to indicate the status of the change request based on values defined by the service provider. Most often a read-only property. Some possible values may include: 'Submitted', 'Done', 'InProgress', etc.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStatus();

    @OslcName("closed")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "closed")
    @OslcDescription("Whether or not the Change Request is completely done, no further fixes or fix verification is needed.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isClosed();

    @OslcName("inProgress")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "inProgress")
    @OslcDescription("Whether or not the Change Request in a state indicating that active work is occurring. If oslc_cm:inprogress is true, then oslc_cm:fixed and oslc_cm:closed must also be false")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isInProgress();

    @OslcName("fixed")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "fixed")
    @OslcDescription("Whether or not the Change Request has been fixed.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isFixed();

    @OslcName("approved")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "approved")
    @OslcDescription("Whether or not the Change Request has been approved.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isApproved();

    @OslcName("reviewed")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "reviewed")
    @OslcDescription("Whether or not the Change Request has been reviewed.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isReviewed();

    @OslcName("verified")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "verified")
    @OslcDescription("Whether or not the resolution or fix of the Change Request has been verified.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isVerified();

    @OslcName("relatedChangeRequest")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "relatedChangeRequest")
    @OslcDescription("This relationship is loosely coupled and has no specific meaning. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getRelatedChangeRequest();

    @OslcName("affectsPlanItem")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "affectsPlanItem")
    @OslcDescription("Change request affects a plan item. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getAffectsPlanItem();

    @OslcName("affectedByDefect")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "affectedByDefect")
    @OslcDescription("Change request is affected by a reported defect. It is likely that the target resource will be an oslc_cm:ChangeRequest but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_cmDomainConstants.DEFECT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAffectedByDefect();

    @OslcName("tracksRequirement")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "tracksRequirement")
    @OslcDescription("Tracks the associated Requirement or Requirement ChangeSet resources. It is likely that the target resource will be an oslc_rm:Requirement but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_rmDomainConstants.REQUIREMENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getTracksRequirement();

    @OslcName("implementsRequirement")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "implementsRequirement")
    @OslcDescription("Implements associated Requirement. It is likely that the target resource will be an oslc_rm:Requirement but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_rmDomainConstants.REQUIREMENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getImplementsRequirement();

    @OslcName("affectsRequirement")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "affectsRequirement")
    @OslcDescription("Change request affecting a Requirement. It is likely that the target resource will be an oslc_rm:Requirement but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_rmDomainConstants.REQUIREMENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAffectsRequirement();

    @OslcName("tracksChangeSet")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "tracksChangeSet")
    @OslcDescription("Tracks SCM change set resource. It is likely that the target resource will be an oslc_scm:ChangeSet but that is not necessarily the case.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_configDomainConstants.CHANGESET_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getTracksChangeSet();

    @OslcName("parent")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "parent")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_cmDomainConstants.CHANGEREQUEST_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getParent();

    @OslcName("priority")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "priority")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_cmDomainConstants.PRIORITY_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getPriority();

    @OslcName("state")
    @OslcPropertyDefinition(Oslc_cmVocabularyConstants.CHANGE_MANAGEMENT_VOCAB_NAMSPACE + "state")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_cmDomainConstants.STATE_TYPE})
    @OslcReadOnly(false)
    public Link getState();

    @OslcName("authorizer")
    @OslcPropertyDefinition(Oslc_cmDomainConstants.CHANGE_MANAGEMENT_SHAPES_NAMSPACE + "authorizer")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.AGENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getAuthorizer();


    public void setShortTitle(final String shortTitle );
    public void setDescription(final String description );
    public void setTitle(final String title );
    public void setIdentifier(final String identifier );
    public void setSubject(final Set<String> subject );
    public void setCreator(final Set<Link> creator );
    public void setContributor(final Set<Link> contributor );
    public void setCreated(final Date created );
    public void setModified(final Date modified );
    public void setServiceProvider(final Set<Link> serviceProvider );
    public void setInstanceShape(final Set<Link> instanceShape );
    public void setDiscussedBy(final Link discussedBy );
    public void setCloseDate(final Date closeDate );
    public void setStatus(final String status );
    public void setClosed(final Boolean closed );
    public void setInProgress(final Boolean inProgress );
    public void setFixed(final Boolean fixed );
    public void setApproved(final Boolean approved );
    public void setReviewed(final Boolean reviewed );
    public void setVerified(final Boolean verified );
    public void setRelatedChangeRequest(final Set<Link> relatedChangeRequest );
    public void setAffectsPlanItem(final Set<Link> affectsPlanItem );
    public void setAffectedByDefect(final Set<Link> affectedByDefect );
    public void setTracksRequirement(final Set<Link> tracksRequirement );
    public void setImplementsRequirement(final Set<Link> implementsRequirement );
    public void setAffectsRequirement(final Set<Link> affectsRequirement );
    public void setTracksChangeSet(final Set<Link> tracksChangeSet );
    public void setParent(final Set<Link> parent );
    public void setPriority(final Set<Link> priority );
    public void setState(final Link state );
    public void setAuthorizer(final Set<Link> authorizer );
}

