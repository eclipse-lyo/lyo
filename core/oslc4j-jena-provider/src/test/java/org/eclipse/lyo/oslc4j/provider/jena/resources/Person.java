package org.eclipse.lyo.oslc4j.provider.jena.resources;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * A person.
 * @author rherrera
 */
@OslcNamespace("http://locahost:7001/vocabulary/")
@OslcResourceShape(title = "AbstractTypesTest")
public class Person extends AbstractResource {

    private String name;
    private Pet pet;

    @OslcPropertyDefinition("http://locahost:7001/vocabulary/name")
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @OslcPropertyDefinition("http://locahost:7001/vocabulary/pet")
    public Pet getPet() {
        return pet;
    }

    public void setPet(Pet pet) {
        this.pet = pet;
    }
}
