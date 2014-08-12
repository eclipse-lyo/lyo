/*******************************************************************************
 * Copyright (c) 2012, 2014 IBM Corporation.
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
 *     Paul McMahan <pmcmahan@us.ibm.com>        - initial implementation
 *     Samuel Padgett <spadgett@us.ibm.com>      - fix getParameterDefinitions annotations
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.net.URI;
import java.util.Arrays;
import java.util.Date;
import java.util.Set;
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
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcResourceShape(title = "Automation Plan Resource Shape", describes = AutomationConstants.TYPE_AUTOMATION_PLAN)
@OslcNamespace(AutomationConstants.AUTOMATION_NAMESPACE)
/**
 * @see http://open-services.net/wiki/automation/OSLC-Automation-Specification-Version-2.0/#Resource_AutomationPlan
 */
public final class AutomationPlan
extends AbstractResource
{
	private final Set<URI>      contributors                = new TreeSet<URI>();
    private final Set<URI>      creators                    = new TreeSet<URI>();
    private final Set<URI>      rdfTypes                    = new TreeSet<URI>();
    private final Set<String>   subjects                    = new TreeSet<String>();
    private final Set<Property> parameterDefinitions        = new TreeSet<Property>();

    private Date     created;
    private String   description;
    private String   identifier;
    private URI      instanceShape;
    private Date     modified;
    private URI      serviceProvider;
    private String   title;

	public AutomationPlan()
	{
		super();

		rdfTypes.add(URI.create(AutomationConstants.TYPE_AUTOMATION_PLAN));
	}

    public AutomationPlan(final URI about)
     {
         super(about);

		rdfTypes.add(URI.create(AutomationConstants.TYPE_AUTOMATION_PLAN));
     }

    protected URI getRdfType() {
    	return URI.create(AutomationConstants.TYPE_AUTOMATION_PLAN);
    }

    public void addContributor(final URI contributor)
    {
        this.contributors.add(contributor);
    }

    public void addCreator(final URI creator)
    {
        this.creators.add(creator);
    }

    public void addRdfType(final URI rdfType)
    {
        this.rdfTypes.add(rdfType);
    }

    public void addSubject(final String subject)
    {
        this.subjects.add(subject);
    }

    public void addParameterDefinition(final Property parameter)
    {
        this.parameterDefinitions.add(parameter);
    }

    @OslcDescription("The person(s) who are responsible for the work needed to complete the automation plan.")
    @OslcName("contributor")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "contributor")
    @OslcRange(QmConstants.TYPE_PERSON)
    @OslcTitle("Contributors")
    public URI[] getContributors()
    {
        return contributors.toArray(new URI[contributors.size()]);
    }

    @OslcDescription("Timestamp of resource creation.")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "created")
    @OslcReadOnly
    @OslcTitle("Created")
    public Date getCreated()
    {
        return created;
    }

    @OslcDescription("Creator or creators of resource.")
    @OslcName("creator")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "creator")
    @OslcRange(QmConstants.TYPE_PERSON)
    @OslcTitle("Creators")
    public URI[] getCreators()
    {
        return creators.toArray(new URI[creators.size()]);
    }

    @OslcDescription("Descriptive text (reference: Dublin Core) about resource represented as rich text in XHTML content.")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "description")
    @OslcTitle("Description")
    @OslcValueType(ValueType.XMLLiteral)
    public String getDescription()
    {
        return description;
    }

    @OslcDescription("A unique identifier for a resource. Assigned by the service provider when a resource is created. Not intended for end-user display.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "identifier")
    @OslcReadOnly
    @OslcTitle("Identifier")
    public String getIdentifier()
    {
        return identifier;
    }

    @OslcDescription("Resource Shape that provides hints as to resource property value-types and allowed values. ")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "instanceShape")
    @OslcRange(OslcConstants.TYPE_RESOURCE_SHAPE)
    @OslcTitle("Instance Shape")
    public URI getInstanceShape()
    {
        return instanceShape;
    }

    @OslcDescription("Timestamp last latest resource modification.")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "modified")
    @OslcReadOnly
    @OslcTitle("Modified")
    public Date getModified()
    {
        return modified;
    }

    @OslcDescription("The resource type URIs.")
    @OslcName("type")
    @OslcPropertyDefinition(OslcConstants.RDF_NAMESPACE + "type")
    @OslcTitle("Types")
    public URI[] getRdfTypes()
    {
        return rdfTypes.toArray(new URI[rdfTypes.size()]);
    }

    @OslcDescription("The scope of a resource is a URI for the resource's OSLC Service Provider.")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "serviceProvider")
    @OslcRange(OslcConstants.TYPE_SERVICE_PROVIDER)
    @OslcTitle("Service Provider")
    public URI getServiceProvider()
    {
        return serviceProvider;
    }

    @OslcDescription("Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes an additional tag for the resource.")
    @OslcName("subject")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "subject")
    @OslcReadOnly(false)
    @OslcTitle("Subjects")
    public String[] getSubjects()
    {
        return subjects.toArray(new String[subjects.size()]);
    }

    @OslcDescription("Title (reference: Dublin Core) or often a single line summary of the resource represented as rich text in XHTML content.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
    @OslcTitle("Title")
    @OslcValueType(ValueType.XMLLiteral)
    public String getTitle()
    {
        return title;
    }

    @OslcDescription("The parameter definitions for the automation plan.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcName("parameterDefinition")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "parameterDefinition")
    @OslcValueType(ValueType.LocalResource)
    @OslcTitle("Parameter Definitions")
    public Property[] getParameterDefinitions()
    {
        return parameterDefinitions.toArray(new Property[parameterDefinitions.size()]);
    }


    public void setContributors(final URI[] contributors)
    {
        this.contributors.clear();

        if (contributors != null)
        {
            this.contributors.addAll(Arrays.asList(contributors));
        }
    }

    public void setCreated(final Date created)
    {
        this.created = created;
    }

    public void setCreators(final URI[] creators)
    {
        this.creators.clear();

        if (creators != null)
        {
            this.creators.addAll(Arrays.asList(creators));
        }
    }

    public void setDescription(final String description)
    {
        this.description = description;
    }

    public void setIdentifier(final String identifier)
    {
        this.identifier = identifier;
    }

    public void setInstanceShape(final URI instanceShape)
    {
        this.instanceShape = instanceShape;
    }

    public void setModified(final Date modified)
    {
        this.modified = modified;
    }

    public void setRdfTypes(final URI[] rdfTypes)
    {
        this.rdfTypes.clear();

        if (rdfTypes != null)
        {
            this.rdfTypes.addAll(Arrays.asList(rdfTypes));
        }
    }

    public void setServiceProvider(final URI serviceProvider)
    {
        this.serviceProvider = serviceProvider;
    }

    public void setSubjects(final String[] subjects)
    {
        this.subjects.clear();

        if (subjects != null)
        {
            this.subjects.addAll(Arrays.asList(subjects));
        }
    }

    public void setTitle(final String title)
    {
        this.title = title;
    }

    public void setParameterDefinitions(final Property[] parameterDefinitions)
    {
        this.parameterDefinitions.clear();

        if (parameterDefinitions != null)
        {
            this.parameterDefinitions.addAll(Arrays.asList(parameterDefinitions));
        }
    }

}
