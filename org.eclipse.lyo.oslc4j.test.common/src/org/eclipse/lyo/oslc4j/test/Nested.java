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
package org.eclipse.lyo.oslc4j.test;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;

@OslcNamespace(Constants.TEST_NAMESPACE)
@OslcResourceShape(title = "Nested Resource Shape", describes = Constants.TYPE_NESTED)
public final class Nested
	   extends AbstractResource
{
	private String stringProperty;

	public Nested()
	{
		super();
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "stringProperty")
	public String getStringProperty()
	{
		return stringProperty;
	}

	public void setStringProperty(final String stringProperty)
	{
		this.stringProperty = stringProperty;
	}


}
