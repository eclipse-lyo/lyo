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
import java.util.Date;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
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

@OslcResourceShape(title = "Automation Result Resource Shape", describes = AutomationConstants.TYPE_AUTOMATION_RESULT)
@OslcNamespace(AutomationConstants.AUTOMATION_NAMESPACE)
/**
 * @see http://open-services.net/wiki/automation/OSLC-Automation-Specification-Version-2.0/#Resource_AutomationResult
 */
public final class AutomationResult
extends AbstractResource
{
	private final Set<URI>      contributors                = new TreeSet<URI>();
    private final Set<URI>      creators                    = new TreeSet<URI>();
    private final Set<URI>      rdfTypes                    = new TreeSet<URI>();
    private final Set<String>   subjects                    = new TreeSet<String>();
    private final Set<URI>      states                      = new TreeSet<URI>();
    private final Set<URI>      verdicts                    = new TreeSet<URI>();
    private final Set<URI>      contributions               = new TreeSet<URI>();
    private final Set<ParameterInstance> inputParameters    = new TreeSet<ParameterInstance>();
    private final Set<ParameterInstance> outputParameters   = new TreeSet<ParameterInstance>();

    private Date     created;
    private String   identifier;
    private URI      instanceShape;
    private Date     modified;
    private URI      serviceProvider;
    private String   title;
    private URI      desiredState;
    private Link      producedByAutomationRequest;
    private Link      reportsOnAutomationPlan;

	public AutomationResult()
	{
		super();

		rdfTypes.add(URI.create(AutomationConstants.TYPE_AUTOMATION_RESULT));
	}

    public AutomationResult(final URI about)
     {
         super(about);

		rdfTypes.add(URI.create(AutomationConstants.TYPE_AUTOMATION_RESULT));
     }

    protected URI getRdfType() {
    	return URI.create(AutomationConstants.TYPE_AUTOMATION_RESULT);
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

    public void addState(final URI state)
    {
        this.states.add(state);
    }

    public void addVerdict(final URI verdict)
    {
        this.verdicts.add(verdict);
    }

    public void addContribution(final URI contribution)
    {
        this.contributions.add(contribution);
    }

    public void addInputParameter(final ParameterInstance parameter)
    {
        this.inputParameters.add(parameter);
    }

    public void addOutputParameter(final ParameterInstance parameter)
    {
        this.outputParameters.add(parameter);
    }

    @OslcDescription("The person(s) who are responsible for the work needed to complete the automation result.")
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

    @OslcDescription("Used to indicate the state of the automation result based on values defined by the service provider.")
    @OslcOccurs(Occurs.OneOrMany)
    @OslcReadOnly(true)
    @OslcName("state")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "state")
    @OslcTitle("State")
    @OslcAllowedValue({
    	AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_NEW,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_IN_PROGRESS,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_QUEUED,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_CANCELING,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_CANCELED,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_COMPLETE})
    public URI[] getStates()
    {
        return states.toArray(new URI[states.size()]);
    }

    @OslcDescription("A result contribution associated with this automation result.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcName("contribution")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "contribution")
    @OslcTitle("Contribution")
    public URI[] getContributions()
    {
        return contributions.toArray(new URI[contributions.size()]);
    }

    @OslcDescription("Used to indicate the verdict of the automation result based on values defined by the service provider.")
    @OslcOccurs(Occurs.OneOrMany)
    @OslcName("verdict")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "verdict")
    @OslcTitle("Verdict")
    @OslcAllowedValue({
    	AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.VERDICT_PASSED,
    	AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.VERDICT_FAILED,
    	AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.VERDICT_WARNING,
    	AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.VERDICT_ERROR,
    	AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.VERDICT_UNAVAILABLE})
    public URI[] getVerdicts()
    {
        return verdicts.toArray(new URI[verdicts.size()]);
    }

    @OslcDescription("Used to indicate the desired state of the Automation Request based on values defined by the service provider.")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "desiredState")
    @OslcName("desiredState")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcTitle("Desired State")
    @OslcAllowedValue({
    	AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_NEW,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_IN_PROGRESS,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_QUEUED,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_CANCELING,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_CANCELED,
		AutomationConstants.AUTOMATION_NAMESPACE + AutomationConstants.STATE_COMPLETE})
    public URI getDesiredState()
    {
        return desiredState;
    }

    @OslcDescription("Automation Request which produced the Automation Result.")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "producedByAutomationRequest")
    @OslcName("producedByAutomationRequest")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcTitle("Produced By Automation Request")
    public Link getProducedByAutomationRequest()
    {
        return producedByAutomationRequest;
    }

    @OslcDescription("Automation Plan which the Automation Result reports on.")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "reportsOnAutomationPlan")
    @OslcName("reportsOnAutomationPlan")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcTitle("Reports On Automation Plan")
    public Link getReportsOnAutomationPlan()
    {
        return reportsOnAutomationPlan;
    }

    @OslcDescription("A copy of the parameters provided during creation of the Automation Request which produced this Automation Result.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcName("inputParameter")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "inputParameter")
    @OslcReadOnly(true)
    @OslcTitle("Input Parameter")
    public ParameterInstance[] getInputParameters()
    {
        return inputParameters.toArray(new ParameterInstance[inputParameters.size()]);
    }

    @OslcDescription("Automation Result output parameters are parameters associated with the automation execution which produced this Result. This includes the final value of all parameters used to initiate the execution and any additional parameters which may have been created during automation execution by the service provider or external agents.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcName("outputParameter")
    @OslcPropertyDefinition(AutomationConstants.AUTOMATION_NAMESPACE + "outputParameter")
    @OslcTitle("Output Parameter")
    public ParameterInstance[] getOutputParameters()
    {
        return outputParameters.toArray(new ParameterInstance[outputParameters.size()]);
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

    public void setStates(final URI[] states)
    {
        this.states.clear();

        if (states != null)
        {
            this.states.addAll(Arrays.asList(states));
        }
    }

    public void setVerdicts(final URI[] verdicts)
    {
        this.verdicts.clear();

        if (verdicts != null)
        {
            this.verdicts.addAll(Arrays.asList(verdicts));
        }
    }

    public void setContributions(final URI[] contributions)
    {
        this.contributions.clear();

        if (contributions != null)
        {
            this.contributions.addAll(Arrays.asList(contributions));
        }
    }

    public void setDesiredState(final URI desiredState)
    {
        this.desiredState = desiredState;
    }

    public void setProducedByAutomationRequest(final Link producedByAutomationRequest)
    {
        this.producedByAutomationRequest = producedByAutomationRequest;
    }

    public void setReportsOnAutomationPlan(final Link reportsOnAutomationPlan)
    {
        this.reportsOnAutomationPlan = reportsOnAutomationPlan;
    }

    public void setInputParameters(final ParameterInstance[] parameters)
    {
        this.inputParameters.clear();

        if (parameters != null)
        {
            this.inputParameters.addAll(Arrays.asList(parameters));
        }
    }

    public void setOutputParameters(final ParameterInstance[] parameters)
    {
        this.outputParameters.clear();

        if (parameters != null)
        {
            this.outputParameters.addAll(Arrays.asList(parameters));
        }
    }


}
