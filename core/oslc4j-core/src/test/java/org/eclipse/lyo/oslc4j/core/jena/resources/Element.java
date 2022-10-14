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
package org.eclipse.lyo.oslc4j.core.jena.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 *
 * @version $version-stub$
 * @since 2.4.0
 */
@OslcName("Element")
@OslcNamespace("http://locahost:7001/vocabulary")
@OslcResourceShape(title = "ElementTest")
public class Element  extends AbstractResource {

    private String name;

    @OslcPropertyDefinition("http://locahost:7001/vocabulary/name")
    public final String getName() {
        return name;
    }
    /**
     * Sets the name of this artifact.
     * @param name the name of this artifact.
     */
    public final void setName(String name) {
        this.name = name;
    }


}
