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
import org.eclipse.lyo.oslc4j.core.model.XMLLiteral;

@OslcName("TestResourceWithLiterals")
@OslcNamespace(TestResourceWithLiterals.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource", describes = TestResourceWithLiterals.TEST_RESOURCE_TYPE)
public class TestResourceWithLiterals extends AbstractResource {
  public static final String TEST_NAMESPACE = "http://example.com/ns#";
  public static final String TEST_RESOURCE_TYPE = TEST_NAMESPACE + "TestResourceWithLiterals";

  private XMLLiteral description;

  @OslcName("description")
  @OslcPropertyDefinition("http://purl.org/dc/terms/description")
  @OslcDescription(
      "Descriptive text about resource represented as rich text in XHTML content. SHOULD"
          + " include only content that is valid and suitable inside an XHTML <div> element.")
  @OslcOccurs(Occurs.ZeroOrOne)
  @OslcValueType(ValueType.XMLLiteral)
  @OslcReadOnly(false)
  public XMLLiteral getDescription() {
    return description;
  }

  public void setDescription(final XMLLiteral value) {
    this.description = value;
  }

  private String title;

  @OslcName("title")
  @OslcPropertyDefinition("http://purl.org/dc/terms/title")
  @OslcDescription(
      "Descriptive text about resource represented as rich text in XHTML content. SHOULD"
          + " include only content that is valid and suitable inside an XHTML <div> element.")
  @OslcOccurs(Occurs.ZeroOrOne)
  @OslcValueType(ValueType.XMLLiteral)
  @OslcReadOnly(false)
  public String getTitle() {
    return title;
  }

  public void setTitle(final String value) {
    this.title = value;
  }
}
