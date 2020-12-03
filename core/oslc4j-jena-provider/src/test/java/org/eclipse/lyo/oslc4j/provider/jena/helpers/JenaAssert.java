/*******************************************************************************
 * Copyright (c) 2018 Andrew Berezovskyi.
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
 *	   Andrew Berezovskyi    -   initial implementation
 *******************************************************************************/
 package org.eclipse.lyo.oslc4j.provider.jena.helpers;

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
