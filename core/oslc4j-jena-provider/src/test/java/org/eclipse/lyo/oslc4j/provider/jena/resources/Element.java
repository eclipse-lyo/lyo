/*******************************************************************************
 * Copyright (c) 2018 Ricardo Javier Herrera.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Ricardo Javier Herrera    -   initial implementation
 *
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.provider.jena.resources;

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
