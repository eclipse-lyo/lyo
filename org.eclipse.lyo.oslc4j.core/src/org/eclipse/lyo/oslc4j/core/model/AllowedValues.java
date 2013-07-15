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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Allowed Values Resource Shape", describes = OslcConstants.TYPE_ALLOWED_VALUES)
public final class AllowedValues extends AbstractResource {
	private final Collection<String> allowedValues = new ArrayList<String>();

	public AllowedValues() {
	    super();
	}

	public void addAllowedValue(final String allowedValue) {
        this.allowedValues.add(allowedValue);
    }

	@OslcDescription("Value allowed for a property")
    @OslcName("allowedValue")
    @OslcOccurs(Occurs.OneOrMany)
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "allowedValue")
    @OslcReadOnly
    @OslcTitle("Allowed Values")
    public String[] getAllowedValues() {
        return allowedValues.toArray(new String[allowedValues.size()]);
    }

	public void setAllowedValues(final String[] allowedValues) {
	    this.allowedValues.clear();
	    if (allowedValues != null) {
	        this.allowedValues.addAll(Arrays.asList(allowedValues));
	    }
	}
}
