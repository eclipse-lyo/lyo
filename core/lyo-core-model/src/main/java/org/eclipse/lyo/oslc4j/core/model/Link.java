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

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;

import java.net.URI;
import java.util.Objects;

/**
 * Special OSLC link type. Differs from {@link URI} since it can hold a label,
 * expressed in RDF using reification.
 * @see <a href="http://open-services.net/bin/view/Main/OslcCoreSpecAppendixLinks">OSLC Core Specification 2.0, Appendix C: Guidance on Links &amp; Relationships</a>
 */
public class Link extends AbstractReifiedResource<URI> {
    private String label;

    public Link() {
    }

    public Link(URI resource) {
        setValue(resource);
    }

    public Link(URI resource, String label) {
        setValue(resource);
        this.label = label;
    }

    @Override
    public int hashCode() {
        return Objects.hash(getValue());
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Link)) {
            return false;
        }
        final Link link = (Link) o;
        return Objects.equals(this.getValue(), link.getValue());
    }

    @OslcName("title")
    @OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
    /**
     * Gets the link label.
     *
     * @return the label
     */ public String getLabel() {
        return label;
    }

    /**
     * Sets the link label.
     * @param label the label
     */
    public void setLabel(String label) {
        this.label = label;
    }

    @Override
    public String toString() {
        return "Link{to='" + getValue() +
                "'; label='" + label + '\'' +
                '}';
    }
}
