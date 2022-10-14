package org.eclipse.lyo.oslc4j.core.jena.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

/**
 * A dog pet
 * @author rherrera
 */
@OslcNamespace("http://locahost:7001/vocabulary/")
@OslcResourceShape(title = "AbstractTypesTest")
public class Dog extends Animal {

    @Override
    public void eat() {
        System.out.println("Eating like a dog");
    }
    
}
