package org.eclipse.lyo.oslc4j.provider.jena.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

/**
 * A cat pet
 * @author rherrera
 */
@OslcNamespace("http://locahost:7001/vocabulary/")
@OslcResourceShape(title = "AbstractTypesTest")
public class Cat extends Animal {

    @Override
    public void eat() {
        System.out.println("Eating like a cat");
    }

}
