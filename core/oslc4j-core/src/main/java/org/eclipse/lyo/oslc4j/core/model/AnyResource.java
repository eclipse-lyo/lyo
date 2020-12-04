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

/**
 * Represents a resource of any (unknown) type, usually the object of a
 * statement in an {@link IExtendedResource}.
 */
public class AnyResource extends AbstractResource
{
	public AnyResource()
	{
		super();
	}

	public AnyResource(URI about)
	{
		super(about);
	}
}
