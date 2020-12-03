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
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

public abstract class AbstractResource implements IExtendedResource {
	private URI about;
	private Collection<URI> types = new ArrayList<URI>();
	private Map<QName, Object> extendedProperties = new HashMap<QName, Object>();

	protected AbstractResource(final URI about) {
		super();

		this.about = about;
	}

	protected AbstractResource() {
		super();
	}

	@Override
	public final URI getAbout() {
		return about;
	}

	@Override
	public final void setAbout(final URI about) {
		this.about = about;
	}
	
	@Override
	public void setExtendedProperties(final Map<QName, Object> properties)
	{
		this.extendedProperties = properties;
	}

	@Override
	public Map<QName, Object> getExtendedProperties()
	{
		return extendedProperties;
	}

	@Override
	public Collection<URI> getTypes()
	{
		return types;
	}

	@Override
	public void setTypes(final Collection<URI> type)
	{
		this.types = type;
	}

	@Override
	public void addType(final URI type)
	{
		this.types.add(type);
	}
}
