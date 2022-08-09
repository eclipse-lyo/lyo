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

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
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
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;

import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;

import org.eclipse.lyo.oslc.domains.config.Oslc_configDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsDomainConstants;
import org.eclipse.lyo.oslc.domains.FoafDomainConstants;
import org.eclipse.lyo.oslc4j.core.model.OslcDomainConstants;
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;
import org.eclipse.lyo.oslc.domains.OsclVocabularyConstants;
import org.eclipse.lyo.oslc.domains.ProvVocabularyConstants;
import org.eclipse.lyo.oslc.domains.Agent;
import org.eclipse.lyo.oslc.domains.config.Component;
import org.eclipse.lyo.oslc.domains.config.ConceptResource;
import org.eclipse.lyo.oslc.domains.Person;
import org.eclipse.lyo.oslc.domains.config.VersionResource;
// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(Oslc_configDomainConstants.VERSIONRESOURCE_NAMESPACE)
@OslcName(Oslc_configDomainConstants.VERSIONRESOURCE_LOCALNAME)
@OslcResourceShape(title = "VersionResource Shape", describes = Oslc_configDomainConstants.VERSIONRESOURCE_TYPE)
public class VersionResource
    extends AbstractResource
    implements IVersionResource
{
    // Start of user code attributeAnnotation:contributor
    // End of user code
    private Set<Link> contributor = new HashSet<Link>();
    // Start of user code attributeAnnotation:created
    // End of user code
    private Date created;
    // Start of user code attributeAnnotation:creator
    // End of user code
    private Set<Link> creator = new HashSet<Link>();
    // Start of user code attributeAnnotation:description
    // End of user code
    private String description;
    // Start of user code attributeAnnotation:identifier
    // End of user code
    private String identifier;
    // Start of user code attributeAnnotation:isVersionOf
    // End of user code
    private Link isVersionOf;
    // Start of user code attributeAnnotation:modified
    // End of user code
    private Date modified;
    // Start of user code attributeAnnotation:subject
    // End of user code
    private Set<String> subject = new HashSet<String>();
    // Start of user code attributeAnnotation:title
    // End of user code
    private String title;
    // Start of user code attributeAnnotation:committed
    // End of user code
    private Date committed;
    // Start of user code attributeAnnotation:committer
    // End of user code
    private Set<Link> committer = new HashSet<Link>();
    // Start of user code attributeAnnotation:component
    // End of user code
    private Link component;
    // Start of user code attributeAnnotation:versionId
    // End of user code
    private String versionId;
    // Start of user code attributeAnnotation:archived
    // End of user code
    private Boolean archived;
    // Start of user code attributeAnnotation:instanceShape
    // End of user code
    private Set<Link> instanceShape = new HashSet<Link>();
    // Start of user code attributeAnnotation:modifiedBy
    // End of user code
    private Set<Link> modifiedBy = new HashSet<Link>();
    // Start of user code attributeAnnotation:serviceProvider
    // End of user code
    private Set<Link> serviceProvider = new HashSet<Link>();
    // Start of user code attributeAnnotation:shortId
    // End of user code
    private String shortId;
    // Start of user code attributeAnnotation:shortTitle
    // End of user code
    private String shortTitle;
    // Start of user code attributeAnnotation:wasDerivedFrom
    // End of user code
    private Set<Link> wasDerivedFrom = new HashSet<Link>();
    // Start of user code attributeAnnotation:wasRevisionOf
    // End of user code
    private Set<Link> wasRevisionOf = new HashSet<Link>();
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public VersionResource()
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public VersionResource(final URI about)
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        Oslc_configDomainConstants.VERSIONRESOURCE_PATH,
        VersionResource.class);
    }
    
    
    public String toString()
    {
        return toString(false);
    }
    
    public String toString(boolean asLocalResource)
    {
        String result = "";
        // Start of user code toString_init
        // End of user code
    
        if (asLocalResource) {
            result = result + "{a Local VersionResource Resource} - update VersionResource.toString() to present resource as desired.";
            // Start of user code toString_bodyForLocalResource
            // End of user code
        }
        else {
            result = String.valueOf(getAbout());
        }
    
        // Start of user code toString_finalize
        // End of user code
    
        return result;
    }
    
    public void addContributor(final Link contributor)
    {
        this.contributor.add(contributor);
    }
    
    public void addCreator(final Link creator)
    {
        this.creator.add(creator);
    }
    
    public void addSubject(final String subject)
    {
        this.subject.add(subject);
    }
    
    public void addCommitter(final Link committer)
    {
        this.committer.add(committer);
    }
    
    public void addInstanceShape(final Link instanceShape)
    {
        this.instanceShape.add(instanceShape);
    }
    
    public void addModifiedBy(final Link modifiedBy)
    {
        this.modifiedBy.add(modifiedBy);
    }
    
    public void addServiceProvider(final Link serviceProvider)
    {
        this.serviceProvider.add(serviceProvider);
    }
    
    public void addWasDerivedFrom(final Link wasDerivedFrom)
    {
        this.wasDerivedFrom.add(wasDerivedFrom);
    }
    
    public void addWasRevisionOf(final Link wasRevisionOf)
    {
        this.wasRevisionOf.add(wasRevisionOf);
    }
    
    
    // Start of user code getterAnnotation:contributor
    // End of user code
    @OslcName("contributor")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "contributor")
    @OslcDescription("Contributor or contributors to the resource. The link target is usually a foaf:Person or foaf:Agent, but could be any type.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE, FoafDomainConstants.AGENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getContributor()
    {
        // Start of user code getterInit:contributor
        // End of user code
        return contributor;
    }

    // Start of user code getterAnnotation:created
    // End of user code
    @OslcName("created")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "created")
    @OslcDescription("Timestamp of resource creation")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCreated()
    {
        // Start of user code getterInit:created
        // End of user code
        return created;
    }

    // Start of user code getterAnnotation:creator
    // End of user code
    @OslcName("creator")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "creator")
    @OslcDescription("Creator or creators of the resource. The link target is usually a foaf:Person or foaf:Agent, but could be any type")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE, FoafDomainConstants.AGENT_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getCreator()
    {
        // Start of user code getterInit:creator
        // End of user code
        return creator;
    }

    // Start of user code getterAnnotation:description
    // End of user code
    @OslcName("description")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "description")
    @OslcDescription("Descriptive text about resource represented as rich text in XHTML content. SHOULD include only content that is valid and suitable inside an XHTML <div> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getDescription()
    {
        // Start of user code getterInit:description
        // End of user code
        return description;
    }

    // Start of user code getterAnnotation:identifier
    // End of user code
    @OslcName("identifier")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "identifier")
    @OslcDescription("A unique identifier for a resource. Typically read-only and assigned by the service provider when a resource is created. Not typically intended for end-user display.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getIdentifier()
    {
        // Start of user code getterInit:identifier
        // End of user code
        return identifier;
    }

    // Start of user code getterAnnotation:isVersionOf
    // End of user code
    @OslcName("isVersionOf")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "isVersionOf")
    @OslcDescription("The concept resource of which this resource is a version. The subject of this property must be the version resource URI.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_configDomainConstants.CONCEPTRESOURCE_TYPE})
    @OslcReadOnly(false)
    public Link getIsVersionOf()
    {
        // Start of user code getterInit:isVersionOf
        // End of user code
        return isVersionOf;
    }

    // Start of user code getterAnnotation:modified
    // End of user code
    @OslcName("modified")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "modified")
    @OslcDescription("Timestamp of latest resource modification")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getModified()
    {
        // Start of user code getterInit:modified
        // End of user code
        return modified;
    }

    // Start of user code getterAnnotation:subject
    // End of user code
    @OslcName("subject")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "subject")
    @OslcDescription("Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes an additional tag for the resource.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    @OslcTitle("")
    public Set<String> getSubject()
    {
        // Start of user code getterInit:subject
        // End of user code
        return subject;
    }

    // Start of user code getterAnnotation:title
    // End of user code
    @OslcName("title")
    @OslcPropertyDefinition(DctermsVocabularyConstants.DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription("Title of the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle()
    {
        // Start of user code getterInit:title
        // End of user code
        return title;
    }

    // Start of user code getterAnnotation:committed
    // End of user code
    @OslcName("committed")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "committed")
    @OslcDescription("Date and time this version resource was checked in. Absent for mutable (checked out) versions.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCommitted()
    {
        // Start of user code getterInit:committed
        // End of user code
        return committed;
    }

    // Start of user code getterAnnotation:committer
    // End of user code
    @OslcName("committer")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "committer")
    @OslcDescription("The entity that checked in this version.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.PERSON_TYPE, FoafDomainConstants.AGENT_TYPE})
    @OslcReadOnly(true)
    public Set<Link> getCommitter()
    {
        // Start of user code getterInit:committer
        // End of user code
        return committer;
    }

    // Start of user code getterAnnotation:component
    // End of user code
    @OslcName("component")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "component")
    @OslcDescription("The component to which this version belongs. Configuration Management provider should indicate the owning component for each version resource using either this property, or using the membership relationship from the component LDPC.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({Oslc_configDomainConstants.COMPONENT_TYPE})
    @OslcReadOnly(false)
    public Link getComponent()
    {
        // Start of user code getterInit:component
        // End of user code
        return component;
    }

    // Start of user code getterAnnotation:versionId
    // End of user code
    @OslcName("versionId")
    @OslcPropertyDefinition(Oslc_configDomainConstants.CONFIGURATION_MANAGEMENT_NAMSPACE + "versionId")
    @OslcDescription("A short human-readable identifier for the version of a resource. All versioned resources should have this property; where the property is present, this identifier must be unique amongst all currently existing versions of the same concept resource. The subject of this property should be the concept resource URI.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getVersionId()
    {
        // Start of user code getterInit:versionId
        // End of user code
        return versionId;
    }

    // Start of user code getterAnnotation:archived
    // End of user code
    @OslcName("archived")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "archived")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isArchived()
    {
        // Start of user code getterInit:archived
        // End of user code
        return archived;
    }

    // Start of user code getterAnnotation:instanceShape
    // End of user code
    @OslcName("instanceShape")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "instanceShape")
    @OslcDescription("The URI of a Resource Shape that describes the possible properties, occurrence, value types, allowed values and labels. This shape information is useful in displaying the subject resource as well as guiding clients in performing modifications. Instance shapes may be specific to the authenticated user associated with the request that retrieved the resource, the current state of the resource and other factors and thus should not be cached.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getInstanceShape()
    {
        // Start of user code getterInit:instanceShape
        // End of user code
        return instanceShape;
    }

    // Start of user code getterAnnotation:modifiedBy
    // End of user code
    @OslcName("modifiedBy")
    @OslcPropertyDefinition(OsclVocabularyConstants.OSLC_CORE_NAMSPACE + "modifiedBy")
    @OslcDescription("The entity that most recently modified the subject resource. The link target is usually a foaf:Person or foaf:Agent, but could be any type.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRange({FoafDomainConstants.AGENT_TYPE, FoafDomainConstants.PERSON_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getModifiedBy()
    {
        // Start of user code getterInit:modifiedBy
        // End of user code
        return modifiedBy;
    }

    // Start of user code getterAnnotation:serviceProvider
    // End of user code
    @OslcName("serviceProvider")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "serviceProvider")
    @OslcDescription("A link to the resource's OSLC Service Provider. There may be cases when the subject resource is available from a service provider that implements multiple domain specifications, which could result in multiple values for this property.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcReadOnly(false)
    public Set<Link> getServiceProvider()
    {
        // Start of user code getterInit:serviceProvider
        // End of user code
        return serviceProvider;
    }

    // Start of user code getterAnnotation:shortId
    // End of user code
    @OslcName("shortId")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "shortId")
    @OslcDescription("Shorter form of dcterms:identifier for the resource, such as a number.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getShortId()
    {
        // Start of user code getterInit:shortId
        // End of user code
        return shortId;
    }

    // Start of user code getterAnnotation:shortTitle
    // End of user code
    @OslcName("shortTitle")
    @OslcPropertyDefinition(OslcDomainConstants.OSLC_NAMSPACE + "shortTitle")
    @OslcDescription("Shorter form of dcterms:title for the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getShortTitle()
    {
        // Start of user code getterInit:shortTitle
        // End of user code
        return shortTitle;
    }

    // Start of user code getterAnnotation:wasDerivedFrom
    // End of user code
    @OslcName("wasDerivedFrom")
    @OslcPropertyDefinition(ProvVocabularyConstants.PROVENANCE_NAMSPACE + "wasDerivedFrom")
    @OslcDescription("")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_configDomainConstants.CONCEPTRESOURCE_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getWasDerivedFrom()
    {
        // Start of user code getterInit:wasDerivedFrom
        // End of user code
        return wasDerivedFrom;
    }

    // Start of user code getterAnnotation:wasRevisionOf
    // End of user code
    @OslcName("wasRevisionOf")
    @OslcPropertyDefinition(ProvVocabularyConstants.PROVENANCE_NAMSPACE + "wasRevisionOf")
    @OslcDescription("")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcRepresentation(Representation.Reference)
    @OslcRange({Oslc_configDomainConstants.VERSIONRESOURCE_TYPE})
    @OslcReadOnly(false)
    public Set<Link> getWasRevisionOf()
    {
        // Start of user code getterInit:wasRevisionOf
        // End of user code
        return wasRevisionOf;
    }

    // Start of user code setterAnnotation:contributor
    // End of user code
    public void setContributor(final Set<Link> contributor )
    {
        // Start of user code setterInit:contributor
        // End of user code
        this.contributor.clear();
        if (contributor != null)
        {
            this.contributor.addAll(contributor);
        }
        // Start of user code setterFinalize:contributor
        // End of user code
    }

    // Start of user code setterAnnotation:created
    // End of user code
    public void setCreated(final Date created )
    {
        // Start of user code setterInit:created
        // End of user code
        this.created = created;
        // Start of user code setterFinalize:created
        // End of user code
    }

    // Start of user code setterAnnotation:creator
    // End of user code
    public void setCreator(final Set<Link> creator )
    {
        // Start of user code setterInit:creator
        // End of user code
        this.creator.clear();
        if (creator != null)
        {
            this.creator.addAll(creator);
        }
        // Start of user code setterFinalize:creator
        // End of user code
    }

    // Start of user code setterAnnotation:description
    // End of user code
    public void setDescription(final String description )
    {
        // Start of user code setterInit:description
        // End of user code
        this.description = description;
        // Start of user code setterFinalize:description
        // End of user code
    }

    // Start of user code setterAnnotation:identifier
    // End of user code
    public void setIdentifier(final String identifier )
    {
        // Start of user code setterInit:identifier
        // End of user code
        this.identifier = identifier;
        // Start of user code setterFinalize:identifier
        // End of user code
    }

    // Start of user code setterAnnotation:isVersionOf
    // End of user code
    public void setIsVersionOf(final Link isVersionOf )
    {
        // Start of user code setterInit:isVersionOf
        // End of user code
        this.isVersionOf = isVersionOf;
        // Start of user code setterFinalize:isVersionOf
        // End of user code
    }

    // Start of user code setterAnnotation:modified
    // End of user code
    public void setModified(final Date modified )
    {
        // Start of user code setterInit:modified
        // End of user code
        this.modified = modified;
        // Start of user code setterFinalize:modified
        // End of user code
    }

    // Start of user code setterAnnotation:subject
    // End of user code
    public void setSubject(final Set<String> subject )
    {
        // Start of user code setterInit:subject
        // End of user code
        this.subject.clear();
        if (subject != null)
        {
            this.subject.addAll(subject);
        }
        // Start of user code setterFinalize:subject
        // End of user code
    }

    // Start of user code setterAnnotation:title
    // End of user code
    public void setTitle(final String title )
    {
        // Start of user code setterInit:title
        // End of user code
        this.title = title;
        // Start of user code setterFinalize:title
        // End of user code
    }

    // Start of user code setterAnnotation:committed
    // End of user code
    public void setCommitted(final Date committed )
    {
        // Start of user code setterInit:committed
        // End of user code
        this.committed = committed;
        // Start of user code setterFinalize:committed
        // End of user code
    }

    // Start of user code setterAnnotation:committer
    // End of user code
    public void setCommitter(final Set<Link> committer )
    {
        // Start of user code setterInit:committer
        // End of user code
        this.committer.clear();
        if (committer != null)
        {
            this.committer.addAll(committer);
        }
        // Start of user code setterFinalize:committer
        // End of user code
    }

    // Start of user code setterAnnotation:component
    // End of user code
    public void setComponent(final Link component )
    {
        // Start of user code setterInit:component
        // End of user code
        this.component = component;
        // Start of user code setterFinalize:component
        // End of user code
    }

    // Start of user code setterAnnotation:versionId
    // End of user code
    public void setVersionId(final String versionId )
    {
        // Start of user code setterInit:versionId
        // End of user code
        this.versionId = versionId;
        // Start of user code setterFinalize:versionId
        // End of user code
    }

    // Start of user code setterAnnotation:archived
    // End of user code
    public void setArchived(final Boolean archived )
    {
        // Start of user code setterInit:archived
        // End of user code
        this.archived = archived;
        // Start of user code setterFinalize:archived
        // End of user code
    }

    // Start of user code setterAnnotation:instanceShape
    // End of user code
    public void setInstanceShape(final Set<Link> instanceShape )
    {
        // Start of user code setterInit:instanceShape
        // End of user code
        this.instanceShape.clear();
        if (instanceShape != null)
        {
            this.instanceShape.addAll(instanceShape);
        }
        // Start of user code setterFinalize:instanceShape
        // End of user code
    }

    // Start of user code setterAnnotation:modifiedBy
    // End of user code
    public void setModifiedBy(final Set<Link> modifiedBy )
    {
        // Start of user code setterInit:modifiedBy
        // End of user code
        this.modifiedBy.clear();
        if (modifiedBy != null)
        {
            this.modifiedBy.addAll(modifiedBy);
        }
        // Start of user code setterFinalize:modifiedBy
        // End of user code
    }

    // Start of user code setterAnnotation:serviceProvider
    // End of user code
    public void setServiceProvider(final Set<Link> serviceProvider )
    {
        // Start of user code setterInit:serviceProvider
        // End of user code
        this.serviceProvider.clear();
        if (serviceProvider != null)
        {
            this.serviceProvider.addAll(serviceProvider);
        }
        // Start of user code setterFinalize:serviceProvider
        // End of user code
    }

    // Start of user code setterAnnotation:shortId
    // End of user code
    public void setShortId(final String shortId )
    {
        // Start of user code setterInit:shortId
        // End of user code
        this.shortId = shortId;
        // Start of user code setterFinalize:shortId
        // End of user code
    }

    // Start of user code setterAnnotation:shortTitle
    // End of user code
    public void setShortTitle(final String shortTitle )
    {
        // Start of user code setterInit:shortTitle
        // End of user code
        this.shortTitle = shortTitle;
        // Start of user code setterFinalize:shortTitle
        // End of user code
    }

    // Start of user code setterAnnotation:wasDerivedFrom
    // End of user code
    public void setWasDerivedFrom(final Set<Link> wasDerivedFrom )
    {
        // Start of user code setterInit:wasDerivedFrom
        // End of user code
        this.wasDerivedFrom.clear();
        if (wasDerivedFrom != null)
        {
            this.wasDerivedFrom.addAll(wasDerivedFrom);
        }
        // Start of user code setterFinalize:wasDerivedFrom
        // End of user code
    }

    // Start of user code setterAnnotation:wasRevisionOf
    // End of user code
    public void setWasRevisionOf(final Set<Link> wasRevisionOf )
    {
        // Start of user code setterInit:wasRevisionOf
        // End of user code
        this.wasRevisionOf.clear();
        if (wasRevisionOf != null)
        {
            this.wasRevisionOf.addAll(wasRevisionOf);
        }
        // Start of user code setterFinalize:wasRevisionOf
        // End of user code
    }

}
