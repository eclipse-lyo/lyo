// Start of user code Copyright
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
// End of user code

package org.eclipse.lyo.store.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

// Start of user code imports
// End of user code

@OslcNamespace(Nsp1DomainConstants.WITHBLANKRESOURCE_NAMESPACE)
@OslcName(Nsp1DomainConstants.WITHBLANKRESOURCE_LOCALNAME)
@OslcResourceShape(title = "WithBlankResource Resource Shape", describes = Nsp1DomainConstants.WITHBLANKRESOURCE_TYPE)
public interface IWithBlankResource
{


    @OslcName("relatesToBlankResource")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "relatesToBlankResource")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.LocalResource)
    @OslcRange({Nsp1DomainConstants.BLANKRESOURCE_TYPE})
    @OslcReadOnly(false)
    public BlankResource getRelatesToBlankResource();

    @OslcName("stringProperty")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "stringProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStringProperty();


    public void setRelatesToBlankResource(final BlankResource relatesToBlankResource );
    public void setStringProperty(final String stringProperty );
}

