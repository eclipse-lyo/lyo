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

package org.eclipse.lyo.oslc.domains;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

// Start of user code imports
// End of user code

@OslcNamespace(RdfsDomainConstants.CLASS_NAMESPACE)
@OslcName(RdfsDomainConstants.CLASS_LOCALNAME)
@OslcResourceShape(title = "Class Shape", describes = RdfsDomainConstants.CLASS_TYPE)
public interface IRdfsClass {

    @OslcName("subClassOf")
    @OslcPropertyDefinition(RdfsVocabularyConstants.RDFS_NAMSPACE + "subClassOf")
    @OslcDescription("used to state that all the instances of one class are instances of another")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({RdfsDomainConstants.CLASS_TYPE})
    @OslcReadOnly(false)
    public Link getSubClassOf();

    public void setSubClassOf(final Link subClassOf);
}
