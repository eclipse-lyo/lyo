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

package org.eclipse.lyo.shacl;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

// Start of user code imports
// End of user code

@OslcNamespace(ShDomainConstants.VALIDATIONRESULT_NAMESPACE)
@OslcName(ShDomainConstants.VALIDATIONRESULT_LOCALNAME)
@OslcResourceShape(title = "ValidationResult Resource Shape", describes = ShDomainConstants.VALIDATIONRESULT_TYPE)
public interface IValidationResult
{


    @OslcName("resultPath")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "resultPath")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcReadOnly(false)
    public URI getResultPath();

    @OslcName("focusNode")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "focusNode")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcReadOnly(false)
    public URI getFocusNode();

    @OslcName("resultMessage")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "resultMessage")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getMessage();

    @OslcName("resultSeverity")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "resultSeverity")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcReadOnly(false)
    @OslcAllowedValue({"http://www.w3.org/ns/shacl#Info", "http://www.w3.org/ns/shacl#Warning", "http://www.w3.org/ns/shacl#Violation"})
    public URI getResultSeverity();


    public void setResultPath(final URI resultPath );
    public void setFocusNode(final URI focusNode );
    public void setMessage(final String message );
    public void setResultSeverity(final URI resultSeverity );
}

