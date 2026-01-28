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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

/**
 * Holds multiple arbitrary reified statements associated with a link.
 */
public class MultiStatementLink extends AbstractReifiedResource<URI> {

    /**
     * A set of predicate-object statements associated with the link.
     */
    private final Map<QName, Object> statements = new HashMap<>();

    public MultiStatementLink() {
    }
    
    public MultiStatementLink(URI resource) {
        setValue(resource);
    }

    public void setStatements(final Map<QName, Object> statements) {
        this.statements.clear();
        if (statements != null) {
            this.statements.putAll(statements);
        }
    }

    public Map<QName, Object> getStatements() {
        return Collections.unmodifiableMap(statements);
    }

    public void addStatement(QName qName, Object object) {
        statements.put(qName, object);
    }
}
