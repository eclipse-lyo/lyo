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
 package org.eclipse.lyo.oslc4j.core.jena.helpers;

import org.apache.jena.rdf.model.Model;
import org.assertj.core.api.AbstractAssert;

/**
 * @version $version-stub$
 * @since 2.4.0
 */
public class JenaAssert extends AbstractAssert<JenaAssert, Model> {
    public JenaAssert(final Model model) {
        super(model, JenaAssert.class);
    }

    public static JenaAssert assertThat(Model m) {
        return new JenaAssert(m);
    }

    public JenaAssert isomorphicWith(Model m) {
        isNotNull();

        if(!actual.isIsomorphicWith(m)) {
             failWithMessage("Expected models to be isomorphic, but they differ too much"
                                     + ".\n\nEXPECTED:\n\n%s\n***\n\nACTUAL:\n\n%s\n===",
                             RDFHelper.toTurtleString(m), RDFHelper.toTurtleString(actual));
        }

        return this;
    }

}
