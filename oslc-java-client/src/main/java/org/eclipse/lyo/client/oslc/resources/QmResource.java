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
 *     Paul McMahan         - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.net.URI;
import java.util.Arrays;
import java.util.Date;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

/**
 * @see http://open-services.net/bin/view/Main/QmSpecificationV2
 */
public abstract class QmResource
       extends AbstractResource
{
    private final Set<URI>      rdfTypes                    = new TreeSet<URI>();

    private Date     created;
    private String   identifier;
    private URI      instanceShape;
    private Date     modified;
    private URI      serviceProvider;
    private String   title;

    public QmResource()
     {
         super();

         rdfTypes.add(getRdfType());
     }

     public QmResource(final URI about)
     {
         super(about);

         rdfTypes.add(getRdfType());
     }

    protected abstract URI getRdfType();

    public void addRdfType(final URI rdfType)
    {
        this.rdfTypes.add(rdfType);
    }

    @OslcDescription("Timestamp of resource creation.")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "created")
    @OslcReadOnly
    @OslcTitle("Created")
    public Date getCreated()
    {
        return created;
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

    @OslcDescription("Title (reference: Dublin Core) or often a single line summary of the resource represented as rich text in XHTML content.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
    @OslcTitle("Title")
    @OslcValueType(ValueType.XMLLiteral)
    public String getTitle()
    {
        return title;
    }

    public void setCreated(final Date created)
    {
        this.created = created;
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

    public void setTitle(final String title)
    {
        this.title = title;
    }

}
