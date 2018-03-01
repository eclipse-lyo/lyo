/**
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
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
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.consumer.rdf;

import java.io.IOException;
import java.io.StringWriter;

import org.apache.jena.rdf.model.Model;
import org.apache.log4j.Logger;


/**
 *
 * utility methods for serializing an rdf model in jena in a requested syntax
 *
 * @author Omar
 *
 */
public class RdfUtil {
    static Logger logger = Logger.getLogger(RdfUtil.class);

    static public String modelToRdfXml(Model model) throws IOException {
        return modelToString(model, "RDF/XML");
    }

    static public String modelToNTriple(Model model) throws IOException {
        return modelToString(model, "N-TRIPLE");
    }

    /**
     * serialize given rdf model in the requested syntax
     *
     * @param model
     *            model to be serialized
     * @param syntax
     *            serialization syntax
     * @return the seralization of the model as a string
     * @throws IOException
     */
    static public String modelToString(Model model, String syntax) throws IOException {
        // try "N-TRIPLE" or "TURTLE"
        final StringWriter out = new StringWriter();
        model.write(out, syntax);
        final String result = out.toString();
        out.close();
        return result;
    }

}
