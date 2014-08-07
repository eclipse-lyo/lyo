/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
 *	   Samuel Padgett		- allow values other than String
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Allowed Values Resource Shape", describes = OslcConstants.TYPE_ALLOWED_VALUES)
public final class AllowedValues extends AbstractResource {
	private static final QName PROPERTY_ALLOWED_VALUE = new QName(OslcConstants.OSLC_CORE_NAMESPACE, "allowedValue");

	public AllowedValues() {
		super();
	}
	
	public Collection<?> getValues() {
		Collection<?> allowedValues = (Collection<?>) getExtendedProperties().get(PROPERTY_ALLOWED_VALUE);
		if (allowedValues == null) {
			return Collections.emptyList();
		}
		
		return allowedValues;
	}
	
	public void setValues(final Collection<? extends Object> values) {
		getExtendedProperties().put(PROPERTY_ALLOWED_VALUE, values);
	}

	/**
	 * @deprecated Use {@link #getValues()}, which allows for values other than String
	 */
	@Deprecated
	public String[] getAllowedValues() {
		// Be compatible with the old behavior and only include String values.
		ArrayList<String> stringValues = new ArrayList<String>();
		Collection<?> values = (Collection<?>) getExtendedProperties().get(PROPERTY_ALLOWED_VALUE);
		for (Object o : values) {
			if (o instanceof String) {
				stringValues.add((String) o);
			}
		}

		return stringValues.toArray(new String[stringValues.size()]);
	}

	/**
	 * @deprecated Use {@link #setValues(Object[])}, which allows for values other than String
	 */
	@Deprecated
	public void setAllowedValues(final String[] allowedValues) {
		getExtendedProperties().put(PROPERTY_ALLOWED_VALUE, allowedValues);
	}
}
