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
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(
        title = "OSLC Prefix Definition Resource Shape",
        describes = OslcConstants.TYPE_PREFIX_DEFINITION)
public class PrefixDefinition extends AbstractResource {
    private String prefix;
    private URI prefixBase;

    public PrefixDefinition() {
        super();
    }

    public PrefixDefinition(final String prefix, final URI prefixBase) {
        this();

        this.prefix = prefix;
        this.prefixBase = prefixBase;
    }

    @OslcDescription("Namespace prefix to be used for this namespace")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "prefix")
    @OslcReadOnly
    @OslcTitle("Prefix")
    public String getPrefix() {
        return prefix;
    }

    @OslcDescription("The base URI of the namespace")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "prefixBase")
    @OslcReadOnly
    @OslcTitle("Prefix Base")
    public URI getPrefixBase() {
        return prefixBase;
    }

    public void setPrefix(final String prefix) {
        this.prefix = prefix;
    }

    public void setPrefixBase(final URI prefixBase) {
        this.prefixBase = prefixBase;
    }
}
