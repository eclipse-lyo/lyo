/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 *	All rights reserved. This program and the accompanying materials
 *	are made available under the terms of the Eclipse Public License v1.0
 *	and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *	
 *	The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *	and the Eclipse Distribution License is available at
 *	http://www.eclipse.org/org/documents/edl-v10.php.
 *	
 *	Contributors:
 *	
 *	   Samuel Padgett - initial API and implementation
 *******************************************************************************/
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
