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

package org.eclipse.lyo.validation.shacl;

import com.google.common.collect.ImmutableList;
import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.eclipse.lyo.oslc4j.core.annotation.*;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

/**
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
@OslcNamespace(ShaclConstants.SHACL_CORE_NAMESPACE)
@OslcName("Shape")
@OslcResourceShape(title = "Shacl Resource Shape", describes = ShaclConstants.TYPE_SHACL_SHAPE)
public final class Shape extends AbstractResource {
    //Core Constraints
    private final Map<URI, Property> properties = new HashMap<URI, Property>();
    //Targets
    private URI targetClass;
    private URI targetNode;
    private URI targetSubjectsOf;
    private URI targetObjectsOf;
    private URI isDefinedBy;
    private String label;
    private URI type;
    private boolean isClosed;
    private List<URI> ignoredProperties;

    private boolean readShaclAnnotations = false;

    public Shape() {
        super();
    }

    public Shape(final URI about) {
        super(about);
    }

    @Override
    public String toString() {
        return "Shape [targetClass=" + targetClass + ", properties=" + properties + "]";
    }

    public void addIgnoredProperties(final URI ignoredPropertyPredicate) {
        this.ignoredProperties.add(ignoredPropertyPredicate);
    }

    public void addProperty(final Property property) {
        this.properties.put(property.getPath(), property);
    }

    public void removeProperty(final URI predicate) {
        this.properties.remove(predicate);
    }

    public Property getShaclProperty(URI definition) {
        return properties.get(definition);
    }

    @OslcDescription("Type or types of resource described by this shape")
    @OslcPropertyDefinition(OslcConstants.RDF_NAMESPACE + "type")
    @OslcReadOnly
    @OslcTitle("RDF Type")
    public URI getType() {
        return type;
    }

    public void setType(URI type) {
        this.type = type;
    }

    @OslcDescription("Type or types of resource described by this shape")
    @OslcPropertyDefinition(ShaclConstants.SHACL_CORE_NAMESPACE + "targetClass")
    @OslcReadOnly
    @OslcTitle("targetClass")
    public URI getTargetClass() {
        return targetClass;
    }

    public void setTargetClass(final URI targetClass) {
        this.targetClass = targetClass;
    }

    @OslcDescription("Type or types of resource described by this shape")
    @OslcPropertyDefinition(ShaclConstants.SHACL_CORE_NAMESPACE + "targetSubjectsOf")
    @OslcReadOnly
    @OslcTitle("targetSubjectsOf")
    public URI getTargetSubjectsOf() {
        return targetSubjectsOf;
    }

    public void setTargetSubjectsOf(final URI targetSubjectsOf) {
        this.targetSubjectsOf = targetSubjectsOf;
    }

    @OslcDescription("Type or types of resource described by this shape")
    @OslcPropertyDefinition(ShaclConstants.SHACL_CORE_NAMESPACE + "targetObjectsOf")
    @OslcReadOnly
    @OslcTitle("targetObjectsOf")
    public URI getTargetObjectsOf() {
        return targetObjectsOf;
    }

    public void setTargetObjectsOf(final URI targetObjectsOf) {
        this.targetObjectsOf = targetObjectsOf;
    }

    @OslcDescription("The properties that are allowed or required by this shape")
    @OslcName("property")
    @OslcPropertyDefinition(ShaclConstants.SHACL_CORE_NAMESPACE + "property")
    @OslcRange(ShaclConstants.TYPE_SHACL_PROPERTY)
    @OslcReadOnly
    @OslcTitle("Properties")
    @OslcValueType(ValueType.LocalResource)
    public List<Property> getShaclProperties() {
        return ImmutableList.copyOf(
                properties.values().toArray(new Property[properties.size()]));
    }

    public void setShaclProperties(final Property[] properties) {
        this.properties.clear();
        if (properties != null) {
            for (Property prop : properties) {
                this.properties.put(prop.getPath(), prop);
            }
        }
    }

    @OslcDescription("Specified Is Defined By")
    @OslcPropertyDefinition(OslcConstants.RDFS_NAMESPACE + "isDefinedBy")
    @OslcTitle("isDefinedBy")
    public URI getIsDefinedBy() {
        return isDefinedBy;
    }

    public void setIsDefinedBy(URI isDefinedBy) {
        this.isDefinedBy = isDefinedBy;
    }

    @OslcDescription("Specified Label")
    @OslcPropertyDefinition(OslcConstants.RDFS_NAMESPACE + "label")
    @OslcTitle("label")
    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    @OslcDescription("Focus Node")
    @OslcPropertyDefinition(ShaclConstants.SHACL_CORE_NAMESPACE + "targetNode")
    @OslcReadOnly
    @OslcTitle("targetNode")
    public URI getTargetNode() {
        return targetNode;
    }

    public void setTargetNode(final URI targetNode) {
        this.targetNode = targetNode;
    }

    @OslcDescription(
            "If set to true, the model is not allowed to have any other property apart from " +
                    "those" + " in shapes graph.")
    @OslcPropertyDefinition(ShaclConstants.SHACL_CORE_NAMESPACE + "closed")
    @OslcValueType(ValueType.Boolean)
    @OslcTitle("Closed")
    public boolean isClosed() {
        return isClosed;

    }

    public void setClosed(boolean isClosed) {
        this.isClosed = isClosed;
    }

    @OslcDescription(
            "Optional SHACL list of properties that are also permitted in addition to those " +
                    "explicitly enumerated via sh:property..")
    @OslcPropertyDefinition(ShaclConstants.SHACL_CORE_NAMESPACE + "ignoredProperties")
    @OslcTitle("IgnoredProperties")
    @OslcRdfCollectionType
    public List<URI> getIgnoredProperties() {
        return ignoredProperties;
    }

    public void setIgnoredProperties(List<URI> ignoredProperties) {
        this.ignoredProperties = ignoredProperties;
    }

    public boolean isReadShaclAnnotations() {
        return readShaclAnnotations;
    }

    public void setReadShaclAnnotations(boolean useShaclAnnotations) {
        this.readShaclAnnotations = useShaclAnnotations;
    }

}
