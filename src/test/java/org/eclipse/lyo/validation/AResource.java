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

/**
 * @since 2.3.0
 */

package org.eclipse.lyo.validation;

import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.Date;
import java.util.HashSet;

import javax.xml.datatype.DatatypeConfigurationException;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.ResourceShape;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.core.model.ValueType;
import org.eclipse.lyo.validation.impl.ShaclExValidatorImpl;
import org.eclipse.lyo.validation.shacl.Shape;
import org.eclipse.lyo.validation.shacl.ShaclShapeFactory;
import org.eclipse.lyo.validation.shacl.ValidationReport;
import org.eclipse.lyo.validation.shacl.annotations.ShaclClassType;
import org.eclipse.lyo.validation.shacl.annotations.ShaclDataType;
import org.eclipse.lyo.validation.shacl.annotations.ShaclIn;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxCount;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxExclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxInclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxLength;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinCount;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinExclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinInclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinLength;
import org.eclipse.lyo.validation.shacl.annotations.ShaclPattern;
import org.eclipse.lyo.validation.shacl.annotations.ShaclTargetClass;

/**
 *
 * @author Yash Khatri 
 *
 */

@OslcNamespace(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE)
@OslcName(SampleAdaptorConstants.ARESOURCE)
@OslcResourceShape(title = "AResource Resource Shape",
                   describes = SampleAdaptorConstants.TYPE_ARESOURCE)
@ShaclTargetClass(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + SampleAdaptorConstants.ARESOURCE)
public class AResource extends AbstractResource {
    private static Shape shaclShape = null;
    private static ResourceShape resourceShape = null;
    private BigInteger anIntegerProperty;
    private BigInteger integerProperty2;
    private BigInteger anotherIntegerProperty;
    private String aStringProperty;
    private HashSet<Date> aSetOfDates = new HashSet<Date>();
    private Link aReferenceProperty = new Link();
    private BigInteger integerProperty3;
    private String anotherStringProperty;

    public AResource() throws URISyntaxException {
        super();
    }

    public AResource(final URI about) throws URISyntaxException {
        super(about);
    }

    public void addASetOfDates(final Date aSetOfDates) {
        this.aSetOfDates.add(aSetOfDates);
    }

    public ValidationReport validate()
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException,
            DatatypeConfigurationException, OslcCoreApplicationException, URISyntaxException,
            ParseException, InstantiationException, SecurityException, NoSuchMethodException {
        return new ShaclExValidatorImpl().validate(this);
    }

    @OslcName("anIntegerProperty")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "anIntegerProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Integer)
    @OslcReadOnly(false)
    @ShaclMinCount(0)
    @ShaclMaxCount(0)
    @ShaclDataType(ValueType.Integer)
    @ShaclMaxLength(2)
    @ShaclMinLength(1)
    public BigInteger getAnIntegerProperty() {
        return anIntegerProperty;
    }

    public void setAnIntegerProperty(final BigInteger anIntegerProperty) {
        this.anIntegerProperty = anIntegerProperty;
    }

    @OslcName("integerProperty2")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "integerProperty2")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Integer)
    @OslcReadOnly(false)
    @ShaclMaxExclusive(15)
    @ShaclMinExclusive(5)
    public BigInteger getIntegerProperty2() {
        return integerProperty2;
    }

    public void setIntegerProperty2(BigInteger integerProperty2) {
        this.integerProperty2 = integerProperty2;
    }

    @OslcName("integerProperty3")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "integerProperty3")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Integer)
    @OslcReadOnly(false)
    @ShaclMaxInclusive(15)
    @ShaclMinInclusive(5)
    public BigInteger getIntegerProperty3() {
        return integerProperty3;
    }

    public void setIntegerProperty3(BigInteger integerProperty3) {
        this.integerProperty3 = integerProperty3;
    }

    @OslcName("anotherIntegerProperty")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "anotherIntegerProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Integer)
    @OslcReadOnly(false)
    @ShaclMinCount(1)
    @ShaclMaxCount(1)
    @ShaclDataType(ValueType.Integer)
    @ShaclMaxLength(2)
    @ShaclMinLength(1)
    public BigInteger getAnotherIntegerProperty() {
        return anotherIntegerProperty;
    }

    public void setAnotherIntegerProperty(final BigInteger anotherIntegerProperty) {
        this.anotherIntegerProperty = anotherIntegerProperty;
    }

    @OslcName("aStringProperty")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "aStringProperty")
    @OslcDescription("a Simple Single String Property")
    @OslcValueType(ValueType.String)
    @OslcTitle("a Property")
    @ShaclMinCount(1)
    @ShaclMaxCount(1)
    @ShaclPattern("^B")
    @ShaclMinLength(7)
    @ShaclMaxLength(10)
    public String getAStringProperty() {
        return aStringProperty;
    }

    public void setAStringProperty(final String aStringProperty) {
        this.aStringProperty = aStringProperty;
    }

    @OslcName("aSetOfDates")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "aSetOfDates")
    @OslcOccurs(Occurs.OneOrMany)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    @ShaclMaxCount(2)
    @ShaclMinCount(0)
    public HashSet<Date> getASetOfDates() {
        return aSetOfDates;
    }

    public void setASetOfDates(final HashSet<Date> aSetOfDates) {
        this.aSetOfDates.clear();
        if (aSetOfDates != null) {
            this.aSetOfDates.addAll(aSetOfDates);
        }
    }

    @OslcName("aReferenceProperty")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "aReferenceProperty")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.Resource)
    @OslcRange({SampleAdaptorConstants.TYPE_ANOTHERRESOURCE})
    @OslcReadOnly(false)
    @ShaclClassType(
            SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + SampleAdaptorConstants.ANOTHERRESOURCE)
    public Link getAReferenceProperty() {
        return aReferenceProperty;
    }

    public void setAReferenceProperty(final Link aReferenceProperty) {
        this.aReferenceProperty = aReferenceProperty;
    }

    @OslcName("anotherStringProperty")
    @OslcPropertyDefinition(SampleAdaptorConstants.SAMPLEDOMAIN_NAMSPACE + "anotherStringProperty")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.String)
    @OslcRange({SampleAdaptorConstants.TYPE_ANOTHERRESOURCE})
    @OslcReadOnly(false)
    @ShaclIn({"A", "B", "C"})
    public String getAnotherStringProperty() {
        return anotherStringProperty;
    }

    public void setAnotherStringProperty(String anotherStringProperty) {
        this.anotherStringProperty = anotherStringProperty;
    }

    public Shape getShaclShape()
            throws OslcCoreApplicationException, URISyntaxException, ParseException {
        if (shaclShape == null) {
            return createShaclShape();
        } else {
            return shaclShape;
        }
    }

    public ResourceShape getResourceShape()
            throws OslcCoreApplicationException, URISyntaxException {
        if (resourceShape == null) {
            return createResourceShape();
        } else {
            return resourceShape;
        }
    }

    private Shape createShaclShape()
            throws OslcCoreApplicationException, URISyntaxException, ParseException {
        return ShaclShapeFactory.createShaclShape(AResource.class);
    }

    private ResourceShape createResourceShape()
            throws OslcCoreApplicationException, URISyntaxException {
        return ResourceShapeFactory.createResourceShape("Temp_Service_Base",
                OslcConstants.PATH_RESOURCE_SHAPES, SampleAdaptorConstants.PATH_ARESOURCE,
                AResource.class);
    }
}
