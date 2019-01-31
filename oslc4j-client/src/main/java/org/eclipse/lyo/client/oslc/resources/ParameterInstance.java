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
 *     Paul McMahan <pmcmahan@us.ibm.com>        - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.net.URI;
import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcResourceShape(title = "Parameter Instance Resource Shape", describes = AutomationConstants.TYPE_PARAMETER_INSTANCE)
@OslcNamespace(AutomationConstants.AUTOMATION_NAMESPACE)
/**
 * @see http://open-services.net/wiki/automation/OSLC-Automation-Specification-Version-2.0/#Resource_ParameterInstance
 */
public final class ParameterInstance
extends AbstractResource implements Comparable<ParameterInstance>
{
    private final Set<URI>      rdfTypes                    = new TreeSet<URI>();

    private String   name;
    private String   value;
    private String   description;
    private URI      instanceShape;
    private URI      serviceProvider;

	public ParameterInstance()
	{
		super();

		rdfTypes.add(URI.create(AutomationConstants.TYPE_PARAMETER_INSTANCE));
	}

    public ParameterInstance(final URI about)
    {
         super(about);

		rdfTypes.add(URI.create(AutomationConstants.TYPE_PARAMETER_INSTANCE));
     }

    protected URI getRdfType() {
    	return URI.create(AutomationConstants.TYPE_PARAMETER_INSTANCE);
    }

    public void addRdfType(final URI rdfType)
    {
        this.rdfTypes.add(rdfType);
    }

    @OslcDescription("Descriptive text (reference: Dublin Core) about resource represented as rich text in XHTML content.")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "description")
    @OslcTitle("Description")
    @OslcValueType(ValueType.XMLLiteral)
    public String getDescription()
    {
        return description;
    }

    @OslcDescription("The name of the parameter instance.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "name")
    @OslcTitle("Name")
    public String getName()
    {
        return name;
    }

    @OslcDescription("The value of the parameter.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcPropertyDefinition(OslcConstants.RDF_NAMESPACE + "value")
    @OslcTitle("Value")
    public String getValue()
    {
        return value;
    }

    @OslcDescription("Resource Shape that provides hints as to resource property value-types and allowed values. ")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "instanceShape")
    @OslcRange(OslcConstants.TYPE_RESOURCE_SHAPE)
    @OslcTitle("Instance Shape")
    public URI getInstanceShape()
    {
        return instanceShape;
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

    public void setDescription(final String description)
    {
        this.description = description;
    }

    public void setName(final String name)
    {
        this.name = name;
    }

    public void setValue(final String value)
    {
        this.value = value;
    }

    public void setInstanceShape(final URI instanceShape)
    {
        this.instanceShape = instanceShape;
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

	public int compareTo(ParameterInstance o) {
		return o.getName().compareTo(name);
	}

}
