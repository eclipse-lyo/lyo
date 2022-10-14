package org.eclipse.lyo.oslc4j.provider.jena.resources;

import org.eclipse.lyo.oslc4j.core.model.IExtendedResource;

/**
 * An abstract type.
 * @author rherrera
 */
public interface Pet extends IExtendedResource {

    String getName();
    void setName(String name);
    void eat();

}