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
import java.net.URISyntaxException;
import java.util.HashSet;

import org.eclipse.lyo.oslc4j.core.OSLC4JUtils;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

// Start of user code imports
// End of user code

// Start of user code preClassCode
// End of user code

// Start of user code classAnnotations
// End of user code
@OslcNamespace(ShDomainConstants.VALIDATIONREPORT_NAMESPACE)
@OslcName(ShDomainConstants.VALIDATIONREPORT_LOCALNAME)
@OslcResourceShape(title = "ValidationReport Resource Shape", describes = ShDomainConstants.VALIDATIONREPORT_TYPE)
public class ValidationReport
    extends AbstractResource
    implements IValidationReport
{
    // Start of user code attributeAnnotation:conforms
    // End of user code
    private Boolean conforms;
    // Start of user code attributeAnnotation:result
    // End of user code
    private HashSet<ValidationResult> result = new HashSet<ValidationResult>();
    
    // Start of user code classAttributes
    // End of user code
    // Start of user code classMethods
    // End of user code
    public ValidationReport()
           throws URISyntaxException
    {
        super();
    
        // Start of user code constructor1
        // End of user code
    }
    
    public ValidationReport(final URI about)
           throws URISyntaxException
    {
        super(about);
    
        // Start of user code constructor2
        // End of user code
    }
    
    
    public static ResourceShape createResourceShape() throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape(OSLC4JUtils.getServletURI(),
        OslcConstants.PATH_RESOURCE_SHAPES,
        ShDomainConstants.VALIDATIONREPORT_PATH,
        ValidationReport.class);
    }
    
    
    public void addResult(final ValidationResult result)
    {
        this.result.add(result);
    }
    
    
    // Start of user code getterAnnotation:conforms
    // End of user code
    @OslcName("conforms")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "conforms")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Boolean)
    @OslcReadOnly(false)
    public Boolean isConforms()
    {
        // Start of user code getterInit:conforms
        // End of user code
        return conforms;
    }
    
    // Start of user code getterAnnotation:result
    // End of user code
    @OslcName("result")
    @OslcPropertyDefinition(ShDomainConstants.SHACL_NAMSPACE + "result")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.LocalResource)
    @OslcRange({ShDomainConstants.VALIDATIONRESULT_TYPE})
    @OslcReadOnly(false)
    public HashSet<ValidationResult> getResult()
    {
        // Start of user code getterInit:result
        // End of user code
        return result;
    }
    
    
    // Start of user code setterAnnotation:conforms
    // End of user code
    public void setConforms(final Boolean conforms )
    {
        // Start of user code setterInit:conforms
        // End of user code
        this.conforms = conforms;
    
        // Start of user code setterFinalize:conforms
        // End of user code
    }
    
    // Start of user code setterAnnotation:result
    // End of user code
    public void setResult(final HashSet<ValidationResult> result )
    {
        // Start of user code setterInit:result
        // End of user code
        this.result.clear();
        if (result != null)
        {
            this.result.addAll(result);
        }
    
        // Start of user code setterFinalize:result
        // End of user code
    }

    @Override
    public String toString() {
        return "ValidationReport [conforms=" + conforms + ", result=" + result + "]";
    }
}
