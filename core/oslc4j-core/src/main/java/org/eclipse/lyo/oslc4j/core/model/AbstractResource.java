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
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

public abstract class AbstractResource implements IExtendedResource {
	private URI about;
	private Collection<URI> types = new ArrayList<>();
	private Map<QName, Object> extendedProperties = new HashMap<>();

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
