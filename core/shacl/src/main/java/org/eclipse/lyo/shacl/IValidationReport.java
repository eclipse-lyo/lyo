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

import java.util.HashSet;
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

@OslcNamespace(ShDomainConstants.VALIDATIONREPORT_NAMESPACE)
@OslcName(ShDomainConstants.VALIDATIONREPORT_LOCALNAME)
@OslcResourceShape(
        title = "ValidationReport Resource Shape",
        describes = ShDomainConstants.VALIDATIONREPORT_TYPE)
public interface IValidationReport {

    public void addResult(final ValidationResult result);

    @OslcName("conforms")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "conforms")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isConforms();

    @OslcName("result")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "result")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.LocalResource)
    @OslcRange({ShDomainConstants.VALIDATIONRESULT_TYPE})
    @OslcReadOnly(false)
    public HashSet<ValidationResult> getResult();

    public void setConforms(final Boolean conforms);

    public void setResult(final HashSet<ValidationResult> result);
}
