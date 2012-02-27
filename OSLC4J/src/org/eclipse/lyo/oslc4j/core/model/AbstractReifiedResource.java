/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *     Samuel Padgett - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

/**
 * Abstract implementation of {@link IReifiedResource}.
 */
public abstract class AbstractReifiedResource<T> implements IReifiedResource<T> {
	private T value;
	
	protected AbstractReifiedResource()
	{
	}
	
	protected AbstractReifiedResource(T value)
	{
		this.value = value;
	}
	
	@Override
	public T getValue()
	{
		return value;
	}

	@Override
	public void setValue(T value)
	{
		this.value = value;
	}
}
