/*******************************************************************************
 * Copyright (c) 2013, 2015 IBM Corporation.
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
 *	 Gabriel Ruelas	   - initial API and implementation
 *	 Carlos A Arreola	 - initial API and implementation
 *	 Samuel Padgett	   - avoid unnecessary URISyntaxException
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.net.URI;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
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
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ValueType;


@OslcNamespace(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE)
@OslcResourceShape(title = "Requirement Resource Shape", describes = RmConstants.TYPE_REQUIREMENT)
public class Requirement
	   extends AbstractResource
{

	private String title;
	private String description;
	private String identifier;
	private String   shortTitle;
	private final Set<String>   subjects					= new TreeSet<String>();
	private final Set<URI>	  creators					= new TreeSet<URI>();
	private final Set<URI>	  contributors				= new TreeSet<URI>();
	private Date created;
	private Date modified;
	private final Set<URI>	  rdfTypes					= new TreeSet<URI>();
	private URI	  serviceProvider;
	private URI	  instanceShape;


	// OSLC Links
	private final Set<Link>	 elaboratedBy				= new HashSet<Link>();
	private final Set<Link>	 elaborates		 			= new HashSet<Link>();

	private final Set<Link>	 specifiedBy		   		= new HashSet<Link>();
	private final Set<Link>	 specifies  					= new HashSet<Link>();

	private final Set<Link>	 affectedBy					= new HashSet<Link>();

	private final Set<Link>	 trackedBy			  	  	= new HashSet<Link>();

	private final Set<Link>	 implementedBy				= new HashSet<Link>();

	private final Set<Link>	 validatedBy					= new HashSet<Link>();

	private final Set<Link>	 satisfiedBy					= new HashSet<Link>();
	private final Set<Link>	 satisfies					= new HashSet<Link>();

	private final Set<Link>	 decomposedBy				= new HashSet<Link>();
	private final Set<Link>	 decomposes					= new HashSet<Link>();

	private final Set<Link>	 constrainedBy				= new HashSet<Link>();
	private final Set<Link>	 constrains					= new HashSet<Link>();


	public Requirement()
	{
		super();

		// Only add the type if Requirement is the created object
		if ( ! ( this instanceof RequirementCollection ) ) {
			rdfTypes.add(URI.create(RmConstants.TYPE_REQUIREMENT));
		}
	}

	public Requirement(final URI about)
	{
		super(about);

		// Only add the type if Requirement is the created object
		if ( ! ( this instanceof RequirementCollection ) ) {
			rdfTypes.add(URI.create(RmConstants.TYPE_REQUIREMENT));
		}
	}

	public void addSubject(final String subject)
	{
		this.subjects.add(subject);
	}

	public void addConstrains(final Link constrains)
	{
		this.constrains.add(constrains);
	}

	public void addConstrainedBy(final Link constrainedBy)
	{
		this.constrainedBy.add(constrainedBy);
	}

	public void addDecomposes(final Link decomposes)
	{
		this.decomposes.add(decomposes);
	}

	public void addDecomposedBy(final Link decomposedBy)
	{
		this.decomposedBy.add(decomposedBy);
	}

	public void addSatisfies(final Link satisfies)
	{
		this.satisfies.add(satisfies);
	}

	public void addSatisfiedBy(final Link satisfiedBy)
	{
		this.satisfiedBy.add(satisfiedBy);
	}

	public void addValidatedBy(final Link validatedBy)
	{
		this.validatedBy.add(validatedBy);
	}

	public void addTrackedBy(final Link trackedBy)
	{
		this.trackedBy.add(trackedBy);
	}

	public void addImplementedBy(final Link implementedBy)
	{
		this.implementedBy.add(implementedBy);
	}

	public void addAffectedBy(final Link affectedBy)
	{
		this.affectedBy.add(affectedBy);
	}

	public void addElaboratedBy(final Link elaboratedBy)
	{
		this.elaboratedBy.add(elaboratedBy);
	}

	public void addElaborates(final Link elaborates)
	{
		this.elaborates.add(elaborates);
	}

	public void addSpecifiedBy(final Link specifiedBy)
	{
		this.specifiedBy.add(specifiedBy);
	}

	public void addSpecifies(final Link specifies)
	{
		this.specifies.add(specifies);
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

	@OslcDescription("Tag or keyword for a resource. Each occurrence of a dcterms:subject property denotes an additional tag for the resource.")
	@OslcName("subject")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "subject")
	@OslcReadOnly(false)
	@OslcTitle("Subjects")
	public String[] getSubjects()
	{
		return subjects.toArray(new String[subjects.size()]);
	}

	@OslcDescription("The subject is elaborated by the object.")
	@OslcName("elaboratedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "elaboratedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Elaborated By")
	public Link[] getElaboratedBy()
	{
		return elaboratedBy.toArray(new Link[elaboratedBy.size()]);
	}

	@OslcDescription("The object is elaborated by the subject.")
	@OslcName("elaborates")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "elaborates")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Elaborates")
	public Link[] getElaborates()
	{
		return elaborates.toArray(new Link[elaborates.size()]);
	}

	@OslcDescription("The subject is specified by the object.")
	@OslcName("specifiedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "specifiedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Specified By")
	public Link[] getSpecifiedBy()
	{
		return specifiedBy.toArray(new Link[specifiedBy.size()]);
	}

	@OslcDescription("The object is specified by the subject.")
	@OslcName("specifies")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "specifies")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Specifies")
	public Link[] getSpecifies()
	{
		return specifies.toArray(new Link[specifies.size()]);
	}


	@OslcDescription("Resource, such as a change request, which implements this requirement.")
	@OslcName("implementedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "implementedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Implemented By")
	public Link[] getImplementedBy()
	{
		return implementedBy.toArray(new Link[implementedBy.size()]);
	}

	@OslcDescription("Requirement is affected by a resource, such as a defect or issue.")
	@OslcName("affectedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "affectedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Affected By")
	public Link[] getAffectedBy()
	{
		return affectedBy.toArray(new Link[affectedBy.size()]);
	}

	@OslcDescription("Resource, such as a change request, which tracks this requirement.")
	@OslcName("trackedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "trackedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("tracked By")
	public Link[] getTrackedBy()
	{
		return trackedBy.toArray(new Link[trackedBy.size()]);
	}

	@OslcDescription("Resource, such as a test case, which validates this requirement.")
	@OslcName("validatedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "validatedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Validated By")
	public Link[] getValidatedBy()
	{
		return validatedBy.toArray(new Link[validatedBy.size()]);
	}

	@OslcDescription("The subject is satisfied by the object.")
	@OslcName("satisfiedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "satisfiedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Satisfied By")
	public Link[] getSatisfiedBy()
	{
		return satisfiedBy.toArray(new Link[satisfiedBy.size()]);
	}

	@OslcDescription("The object is satisfied by the subject.")
	@OslcName("satisfies")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "satisfies")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Satisfies")
	public Link[] getSatisfies()
	{
		return satisfies.toArray(new Link[satisfies.size()]);
	}

	@OslcDescription("The subject is decomposed by the object.")
	@OslcName("decomposedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "decomposedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("DecomposedBy")
	public Link[] getDecomposedBy()
	{
		return decomposedBy.toArray(new Link[decomposedBy.size()]);
	}

	@OslcDescription("The object is decomposed by the subject.")
	@OslcName("decomposes")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "decomposes")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Decomposes")
	public Link[] getDecomposes()
	{
		return decomposes.toArray(new Link[decomposes.size()]);
	}

	@OslcDescription("The subject is constrained by the object.")
	@OslcName("constrainedBy")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "constrainedBy")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("ConstrainedBy")
	public Link[] getConstrainedBy()
	{
		return constrainedBy.toArray(new Link[constrainedBy.size()]);
	}

	@OslcDescription("The object is constrained by the subject.")
	@OslcName("constrains")
	@OslcPropertyDefinition(RmConstants.REQUIREMENTS_MANAGEMENT_NAMESPACE + "constrains")
	@OslcRange(RmConstants.TYPE_REQUIREMENT)
	@OslcReadOnly(false)
	@OslcTitle("Constrains")
	public Link[] getConstrains()
	{
		return constrains.toArray(new Link[constrains.size()]);
	}

	@OslcDescription("The person(s) who are responsible for the work needed to complete the change request.")
	@OslcName("contributor")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "contributor")
	@OslcRange(RmConstants.TYPE_PERSON)
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
	@OslcRange(RmConstants.TYPE_PERSON)
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

	@OslcDescription("Short name identifying a resource, often used as an abbreviated identifier for presentation to end-users.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "shortTitle")
	@OslcTitle("Short Title")
	@OslcValueType(ValueType.XMLLiteral)
	public String getShortTitle()
	{
		return shortTitle;
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

	public void setConstrains(final Link[] constrains)
	{
		this.constrains.clear();

		if (constrains != null)
		{
			this.constrains.addAll(Arrays.asList(constrains));
		}
	}

	public void setConstrainedBy(final Link[] constrainedBy)
	{
		this.constrainedBy.clear();

		if (constrainedBy != null)
		{
			this.constrainedBy.addAll(Arrays.asList(constrainedBy));
		}
	}

	public void setDecomposes(final Link[] decomposes)
	{
		this.affectedBy.clear();

		if (decomposes != null)
		{
			this.decomposes.addAll(Arrays.asList(decomposes));
		}
	}

	public void setDecomposedBy(final Link[] decomposedBy)
	{
		this.decomposedBy.clear();

		if (decomposedBy != null)
		{
			this.decomposedBy.addAll(Arrays.asList(decomposedBy));
		}
	}

	public void setSatisfies(final Link[] satisfies)
	{
		this.satisfies.clear();

		if (satisfies != null)
		{
			this.satisfies.addAll(Arrays.asList(satisfies));
		}
	}

	public void setSatisfiedBy(final Link[] satisfiedBy)
	{
		this.satisfiedBy.clear();

		if (satisfiedBy != null)
		{
			this.satisfiedBy.addAll(Arrays.asList(satisfiedBy));
		}
	}

	public void setValidatedBy(final Link[] validatedBy)
	{
		this.validatedBy.clear();

		if (validatedBy != null)
		{
			this.validatedBy.addAll(Arrays.asList(validatedBy));
		}
	}

	public void setTrackedBy(final Link[] trackedBy)
	{
		this.trackedBy.clear();

		if (trackedBy != null)
		{
			this.trackedBy.addAll(Arrays.asList(trackedBy));
		}
	}

	public void setAffectedBy(final Link[] affectedBy)
	{
		this.affectedBy.clear();

		if (affectedBy != null)
		{
			this.affectedBy.addAll(Arrays.asList(affectedBy));
		}
	}

	public void setImplementedBy(final Link[] implementedBy)
	{
		this.implementedBy.clear();

		if (implementedBy != null)
		{
			this.implementedBy.addAll(Arrays.asList(implementedBy));
		}
	}

	public void setElaboratedBy(final Link[] elaboratedBy)
	{
		this.elaboratedBy.clear();

		if (elaboratedBy != null)
		{
			this.elaboratedBy.addAll(Arrays.asList(elaboratedBy));
		}
	}

	public void setElaborates(final Link[] elaborates)
	{
		this.elaborates.clear();

		if (elaborates != null)
		{
			this.elaborates.addAll(Arrays.asList(elaborates));
		}
	}

	public void setSpecifiedBy(final Link[] specifiedBy)
	{
		this.specifiedBy.clear();

		if (specifiedBy != null)
		{
			this.specifiedBy.addAll(Arrays.asList(specifiedBy));
		}
	}


	public void setSpecifies(final Link[] specifies)
	{
		this.specifies.clear();

		if (specifies != null)
		{
			this.specifies.addAll(Arrays.asList(specifies));
		}
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

	public void setShortTitle(final String shortTitle)
	{
		this.shortTitle = shortTitle;
	}


	public void setTitle(final String title)
	{
		this.title = title;
	}


	public void setSubjects(final String[] subjects)
	{
		this.subjects.clear();

		if (subjects != null)
		{
			this.subjects.addAll(Arrays.asList(subjects));
		}
	}

}
