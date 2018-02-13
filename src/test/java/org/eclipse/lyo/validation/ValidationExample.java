/*-*****************************************************************************
 * Copyright (c) 2017 Yash Khatri.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *
 * Contributors:
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/

package org.eclipse.lyo.validation;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.util.FileUtils;
import org.eclipse.lyo.validation.impl.ValidatorImpl;
import org.eclipse.lyo.validation.model.ResourceModel;
import org.eclipse.lyo.validation.model.ValidationResultModel;

/**
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
public class ValidationExample {

    /**
     * Loads an example SHACL file and validates all focus nodes against all shapes.
     */
    public static void main(String[] args) throws Exception {

        // Load the main data model
        Model dataModel = ModelFactory.createDefaultModel();
        dataModel.read(ValidationExample.class.getResourceAsStream("/aResource-Data.ttl"),
                "urn:dummy", FileUtils.langTurtle);
        dataModel.write(System.out, "TURTLE");

        // Load the main shape model
        Model shapeModel = ModelFactory.createDefaultModel();
        shapeModel.read(ValidationExample.class.getResourceAsStream("/aResource-Shape.ttl"),
                "urn:dummy", FileUtils.langTurtle);
        shapeModel.write(System.out, "TURTLE");

        // Perform the validation of the data model against the shapes model. You can have shape
        // and data in same model.
        ValidationResultModel vr = new ValidatorImpl().validate(dataModel, shapeModel);
        for (ResourceModel rm : vr.getInvalidResources()) {

            if (!rm.getResult().isValid()) {
                System.out.println(rm.getResult().show());
            } else {
                System.out.println("Resource is Valid");
            }
        }
    }
}
