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
package org.eclipse.lyo.oslc4j.provider.jena.test.resources;

import org.eclipse.lyo.oslc4j.core.annotation.*;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcName("TestX")
@OslcNamespace(TestResourceWithXMLLiteral.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = TestResourceWithXMLLiteral.TEST_RESOURCE_TYPE)
public class TestResourceWithXMLLiteral extends AbstractResource {
	public final static String TEST_NAMESPACE = "http://example.com/ns#";
	public final static String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "TestX";

    private String description;

    @OslcName("description")
    @OslcPropertyDefinition("http://purl.org/dc/terms/description")
    @OslcDescription("Descriptive text about resource represented as rich text in XHTML content. SHOULD include only content that is valid and suitable inside an XHTML <div> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getDescription()
    {
        // Start of user code getterInit:description
        // End of user code
        return description;
    }

    public void setDescription(final String description )
    {
        // Start of user code setterInit:description
        // End of user code
        this.description = description;

        // Start of user code setterFinalize:description
        // End of user code
    }
}
