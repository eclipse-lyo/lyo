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

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;

import org.eclipse.lyo.samples.bugzilla.URLStrategy;

import thewebsemantic.Namespace;
import thewebsemantic.RdfProperty;
import thewebsemantic.RdfType;

import com.j2bugzilla.base.Bug;
import com.j2bugzilla.base.Bug.Priority;
import com.j2bugzilla.base.Bug.Status;
import com.j2bugzilla.base.BugFactory;
import com.j2bugzilla.base.BugzillaException;
import com.j2bugzilla.base.ConnectionException;

/**
 * An OSLC-CM ChangeRequest with some Bugzilla-specific properties.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 *
 */
@Namespace("http://open-services.net/ns/cm#")
@RdfType("ChangeRequest")
public class BugzillaChangeRequest extends ChangeRequest {
	@RdfProperty("http://www.bugzilla.org/rdf#product")
	private String product = null;
	
	@RdfProperty("http://www.bugzilla.org/rdf#component")
	private String component = null;
	
	@RdfProperty("http://www.bugzilla.org/rdf#version")
	private String version = null;
	
	@RdfProperty("http://www.bugzilla.org/rdf#priority")
	private String priority = null;
	
	@RdfProperty("http://www.bugzilla.org/rdf#platform")
	private String platform = null;
	
	@RdfProperty("http://www.bugzilla.org/rdf#opsys")
	private String operatingSystem = null;
	
	/**
	 * Converts a {@link Bug} to an OSLC-CM ChangeRequest.
	 * 
	 * @param bug
	 *            the bug
	 * @return the ChangeRequest to be serialized
	 * @throws URISyntaxException
	 *             on errors setting the bug URI
	 * @throws UnsupportedEncodingException
	 */
	public static BugzillaChangeRequest fromBug(Bug bug)
			throws URISyntaxException, UnsupportedEncodingException {
		BugzillaChangeRequest cr = new BugzillaChangeRequest();
		cr.setIdentifier(bug.getID());
		cr.setTitle(bug.getSummary());
		
		// j2bugzilla does not handle all status values properly.
		//cr.setStatus(bug.getStatus());
		cr.setStatus((String) bug.getParameterMap().get("status"));

		Object assignedTo = bug.getParameterMap().get("assigned_to");
		if (assignedTo != null) {
			Person contributor = new Person();
			String email = assignedTo.toString();
			contributor.setUri(new URI(URLStrategy.getPersonURL(email)));
			contributor.setEmail(email);
			cr.setContributor(contributor);
		}
		
		Date createdDate = (Date) bug.getParameterMap().get("creation_time");
		Calendar createdCal = Calendar.getInstance();
		createdCal.setTime(createdDate);
		cr.setCreated(createdCal);
		
		Date modifiedDate = (Date) bug.getParameterMap().get("last_change_time");
		Calendar modifiedCal = Calendar.getInstance();
		modifiedCal.setTime(modifiedDate);
		cr.setModified(modifiedCal);
		
		cr.setProduct(bug.getProduct());
		cr.setComponent(bug.getComponent());
		
		// Work around a bug in j2bugzilla. Bug.getVersion() results in a class cast exception.
		Object version = bug.getParameterMap().get("version");
		if (version != null) {
			cr.setVersion(version.toString());
		}
		
		try {
			cr.setPriority(bug.getPriority());
		} catch (NumberFormatException e) {
			// Do nothing, priority is not set.
		}
		
		Map<?, ?> internals = bug.getParameterMap();
		cr.setPlatform((String) internals.get("rep_platform"));
		cr.setOperatingSystem((String) internals.get("op_sys"));
		
		return cr;
	}
	
	/**
	 * Creates a {@link Bug} from an OSLC-CM ChangeRequest.
	 * 
	 * @param bug the bug
	 * @return the ChangeRequest to be serialized
	 * @throws BugzillaException 
	 * @throws ConnectionException 
	 * @throws InvalidDescriptionException 
	 * @throws URISyntaxException on errors setting the bug URI
	 */
	public Bug toBug() throws ConnectionException, BugzillaException {
		BugFactory factory = new BugFactory().newBug();
		if (product != null) {
			factory.setProduct(product);
		}
		if (title != null) {
			factory.setSummary(title);
		}
		if (description != null) {
			factory.setDescription(description);
		}
		if (version != null) {
			factory.setVersion(version);
		}
		if (component != null) {
			factory.setComponent(component);
		}
		if (platform != null) {
			factory.setPlatform(platform);
		}
		if (operatingSystem != null) {
			factory.setOperatingSystem(operatingSystem);
		}
		
		return factory.createBug();
	}
	
	public void setIdentifier(int identifier) throws URISyntaxException {
		setIdentifier(Integer.toString(identifier));
		setUri(new URI(URLStrategy.getChangeRequestURL(identifier)));
	}
	
	public String getProduct() {
		return product;
	}
	
	public void setProduct(String product) {
		this.product = product;
	}
	
	public String getComponent() {
		return component;
	}
	
	public void setComponent(String component) {
		this.component = component;
	}
	
	public String getVersion() {
		return version;
	}
	
	public void setVersion(String version) {
		this.version = version;
	}
	
	public String getPriority() {
		return priority;
	}

	public void setPriority(String priority) {
		this.priority = priority;
	}

	public void setPriority(Priority priority) {
		if (priority == null) {
			setPriority((String) null);
		} else {
			setPriority(priority.toString());
		}
	}
	
	public String getPlatform() {
		return platform;
	}
	
	public void setPlatform(String platform) {
		this.platform = platform;
	}
	
	public String getOperatingSystem() {
		return operatingSystem;
	}
	
	public void setOperatingSystem(String operatingSystem) {
		this.operatingSystem = operatingSystem;
	}
	
	public void setStatus(Status status) {
		if (status != null) {
			setStatus(status.toString());
		} else {
			setStatus((String) null);
		}
	}
}
