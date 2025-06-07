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

import java.net.URI;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcName("Test")
@OslcNamespace(TestResource.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = TestResource.TEST_RESOURCE_TYPE)
public class TestResource extends AbstractResource {
    public static final String TEST_NAMESPACE = "http://example.com/ns#";
    public static final String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "Test";

    public TestResource(URI about) {
        super(about);
    }

    public TestResource() {}

    private String _aproperty;

    @OslcName("aproperty")
    @OslcPropertyDefinition(TEST_NAMESPACE + "aproperty")
    @OslcDescription("A test property.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getAproperty() {
        return _aproperty;
    }

    public void setAproperty(String val) {
        _aproperty = val;
    }
}
