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

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNotQueryResult;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

import java.net.URI;

/* Same as TestResource to test cases where TestResource is returned but endpoint is not a query one */
@OslcName("Test")
@OslcNotQueryResult
@OslcNamespace(TestResourceNonQ.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource (not a query result)", describes = TestResourceNonQ.TEST_RESOURCE_TYPE)
public class TestResourceNonQ extends AbstractResource {
	public final static String TEST_NAMESPACE = "http://example.com/ns#";
	public final static String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "Test";

    public TestResourceNonQ(URI about) {
        super(about);
    }

    public TestResourceNonQ() {
    }

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
