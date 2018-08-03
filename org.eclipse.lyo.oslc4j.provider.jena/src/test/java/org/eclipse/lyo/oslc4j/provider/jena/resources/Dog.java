package org.eclipse.lyo.oslc4j.provider.jena.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * A dog pet
 * @author rherrera
 */
@OslcNamespace("http://locahost:7001/vocabulary/")
@OslcResourceShape(title = "AbstractTypesTest")
public class Dog extends AbstractResource implements Pet {

    private String name;
    
    @Override
    @OslcPropertyDefinition("http://locahost:7001/vocabulary/name")
    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public void eat() {
        System.out.println("Eating like a dog");
    }
    
}
