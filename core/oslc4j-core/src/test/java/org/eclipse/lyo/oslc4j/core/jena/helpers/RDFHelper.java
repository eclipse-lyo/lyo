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

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.riot.RDFLanguages;
import org.eclipse.lyo.oslc4j.core.jena.JenaModelHelperTest;

/**
 * @version $version-stub$
 * @since 2.4.0
 */
public class RDFHelper {

    /**
     * Load a Jena model from a JAR resource file
     * @param fileName path relative to '/src/test/resources'
     * @return Jena model
     * @throws IOException if class loader fails to read the resource
     */
    public static Model loadResourceModel(final String fileName) throws IOException {
        final Model model;
        model = ModelFactory.createDefaultModel();
        try (InputStream s = RDFHelper.class.getClassLoader().getResourceAsStream
                (fileName)) {
            RDFDataMgr.read(model, s, RDFLanguages.filenameToLang(fileName));
//            RDFParser.create()
//                     .source(s)
//                     .lang(RDFLanguages.filenameToLang(fileName))
//                     .errorHandler(ErrorHandlerFactory.errorHandlerStrict)
////                     .base("http://example/base")
//                     .parse(graph);

        }
        return model;
    }

    public static String toTurtleString(final Model responsePlan) {
        final StringWriter stringWriter = new StringWriter();
        RDFDataMgr.write(stringWriter, responsePlan, RDFFormat.TURTLE_PRETTY);
        return stringWriter.toString();
    }
}
