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

/**
 * This interface helps model RDF reified statements in plain old Java objects.
 * OSLC commonly uses reification to describe metadata on links, such as labels.
 * The {@link #getValue()} and {@link #setValue(Object)} methods allow you to
 * set the actual object of the triple. All other properties in implementing
 * classes are statements about the statement. These additional properties
 * should have {@link org.eclipse.lyo.oslc4j.core.annotation.OslcName OslcName} and {@link org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition OSLCPropertyDefinition} annotations.
 * See {@link Link} for an example.
 * <p>
 * Note: The parameterized type T must be a {@link java.net.URI URI} to serialize to JSON due
 * to current limitations in the OSLC JSON format.
 *
 * @see AbstractReifiedResource
 * @see <a href="http://www.w3.org/TR/rdf-mt/#Reif">RDF Semantics: Reification</a>
 * @see <a href="http://www.w3.org/TR/rdf-primer/#reification">RDF Primer: Reification</a>
 */
public interface IReifiedResource<T> {
    /**
     * Gets the object of the reified statement.
     *
     * @return the object of the reified statement
     */
    public T getValue();

    /**
     * Sets the object of the reified statement.
     *
     * @param value the object of the reified statement
     */
    public void setValue(T value);
}
