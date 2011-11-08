/*******************************************************************************
 * Copyright (c) 2011 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *  
 *  Contributors:
 *  
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.samples.bugzilla.resources;

import java.net.URI;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;

import thewebsemantic.Namespace;
import thewebsemantic.RdfProperty;

/**
 * An OSLC-CM ChangeRequest.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see <a href="http://open-services.net/bin/view/Main/CmSpecificationV2?sortcol=table;up=#Resource_ChangeRequest">OSLC-CM 2.0 Specification, Resource ChangeRequest</a>
 */
@Namespace("http://open-services.net/ns/cm#")
public class ChangeRequest extends OslcResource {
	@RdfProperty("http://open-services.net/ns/cm#shortTitle")
	protected String shortTitle = null;
	
	@RdfProperty("http://purl.org/dc/terms/identifier")
	protected String identifier = null;
	
	@RdfProperty("http://purl.org/dc/terms/type")
	protected String type = null;

	@RdfProperty("http://purl.org/dc/terms/creator")
	protected Person creator = null;
	
	@RdfProperty("http://purl.org/dc/terms/contributor")
	protected Person contributor = null;

	@RdfProperty("http://purl.org/dc/terms/subject")
	protected String subject = null;

	@RdfProperty("http://purl.org/dc/terms/created")
	protected Calendar created = null;
	
	@RdfProperty("http://purl.org/dc/terms/modified")
	protected Calendar modified = null;
	
	@RdfProperty("http://open-services.net/ns/cm#status")
	protected String status = null;
	
	/*
	 * Links
	 */
	@RdfProperty("http://open-services.net/ns/cm#relatedChangeRequest")
	private Collection<URI> relatedChangeRequest = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#affectsPlanItem")
	private Collection<URI> affectsPlanItem = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#affectedByDefect")
	private Collection<URI> affectedByDefect = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#tracksRequirement")
	private Collection<URI> tracksRequirement = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#implementsRequirement")
	private Collection<URI> implementsRequirement = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#affectsRequirement")
	private Collection<URI> affectsRequirement = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#testedByTestCase")
	private Collection<URI> testedByTestCase = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#affectsTestResult")
	private Collection<URI> affectsTestResult = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#blocksTestExecutionRecord")
	private Collection<URI> blocksTestExecutionRecord = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#relatedTestExecutionRecord")
	private Collection<URI> relatedTestExecutionRecord = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#relatedTestCase")
	private Collection<URI> relatedTestCase = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#relatedTestPlan")
	private Collection<URI> relatedTestPlan = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#relatedTestScript")
	private Collection<URI> relatedTestScript = new ArrayList<URI>();

	@RdfProperty("http://open-services.net/ns/cm#tracksChangeSet")
	private Collection<URI> tracksChangeSet = new ArrayList<URI>();
	
	public String getShortTitle() {
		return shortTitle;
	}

	public void setShortTitle(String shortTitle) {
		this.shortTitle = shortTitle;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

	public String getIdentifier() {
		return identifier;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Person getCreator() {
		return creator;
	}

	public void setCreator(Person creator) {
		this.creator = creator;
	}

	public Person getContributor() {
		return contributor;
	}

	public void setContributor(Person contributor) {
		this.contributor = contributor;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public Calendar getCreated() {
		return created;
	}

	public void setCreated(Calendar created) {
		this.created = created;
	}

	public Calendar getModified() {
		return modified;
	}

	public void setModified(Calendar modified) {
		this.modified = modified;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public Collection<URI> getRelatedChangeRequest() {
		return relatedChangeRequest;
	}

	public Collection<URI> getAffectsPlanItem() {
		return affectsPlanItem;
	}

	public Collection<URI> getAffectedByDefect() {
		return affectedByDefect;
	}

	public Collection<URI> getTracksRequirement() {
		return tracksRequirement;
	}

	public Collection<URI> getImplementsRequirement() {
		return implementsRequirement;
	}

	public Collection<URI> getAffectsRequirement() {
		return affectsRequirement;
	}

	public Collection<URI> getTestedByTestCase() {
		return testedByTestCase;
	}

	public Collection<URI> getAffectsTestResult() {
		return affectsTestResult;
	}

	public Collection<URI> getBlocksTestExecutionRecord() {
		return blocksTestExecutionRecord;
	}

	public Collection<URI> getRelatedTestExecutionRecord() {
		return relatedTestExecutionRecord;
	}

	public Collection<URI> getRelatedTestCase() {
		return relatedTestCase;
	}

	public Collection<URI> getRelatedTestPlan() {
		return relatedTestPlan;
	}

	public Collection<URI> getRelatedTestScript() {
		return relatedTestScript;
	}

	public Collection<URI> getTracksChangeSet() {
		return tracksChangeSet;
	}
}
