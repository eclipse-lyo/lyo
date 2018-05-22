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
 * Contributors:
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/

package org.eclipse.lyo.validation.shacl;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValues;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcMaxSize;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreDuplicatePropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreInvalidPropertyDefinitionException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingAnnotationException;
import org.eclipse.lyo.oslc4j.core.exception.OslcCoreMissingSetMethodException;
import org.eclipse.lyo.oslc4j.core.model.IReifiedResource;
import org.eclipse.lyo.oslc4j.core.model.InheritedMethodAnnotationHelper;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ResourceShapeFactory;
import org.eclipse.lyo.oslc4j.core.model.ValueType;
import org.eclipse.lyo.validation.shacl.annotations.RDFType;
import org.eclipse.lyo.validation.shacl.annotations.RdfsIsDefinedBy;
import org.eclipse.lyo.validation.shacl.annotations.RdfsLabel;
import org.eclipse.lyo.validation.shacl.annotations.ShaclClassType;
import org.eclipse.lyo.validation.shacl.annotations.ShaclClosed;
import org.eclipse.lyo.validation.shacl.annotations.ShaclDataType;
import org.eclipse.lyo.validation.shacl.annotations.ShaclDescription;
import org.eclipse.lyo.validation.shacl.annotations.ShaclDisjoint;
import org.eclipse.lyo.validation.shacl.annotations.ShaclEquals;
import org.eclipse.lyo.validation.shacl.annotations.ShaclGroup;
import org.eclipse.lyo.validation.shacl.annotations.ShaclHasValue;
import org.eclipse.lyo.validation.shacl.annotations.ShaclIgnoredProperties;
import org.eclipse.lyo.validation.shacl.annotations.ShaclIn;
import org.eclipse.lyo.validation.shacl.annotations.ShaclLanguageIn;
import org.eclipse.lyo.validation.shacl.annotations.ShaclLessThan;
import org.eclipse.lyo.validation.shacl.annotations.ShaclLessThanOrEquals;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxCount;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxExclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxInclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMaxLength;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinCount;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinExclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinInclusive;
import org.eclipse.lyo.validation.shacl.annotations.ShaclMinLength;
import org.eclipse.lyo.validation.shacl.annotations.ShaclName;
import org.eclipse.lyo.validation.shacl.annotations.ShaclNode;
import org.eclipse.lyo.validation.shacl.annotations.ShaclOrder;
import org.eclipse.lyo.validation.shacl.annotations.ShaclPattern;
import org.eclipse.lyo.validation.shacl.annotations.ShaclTargetClass;
import org.eclipse.lyo.validation.shacl.annotations.ShaclTargetNode;
import org.eclipse.lyo.validation.shacl.annotations.ShaclTargetObjectsOf;
import org.eclipse.lyo.validation.shacl.annotations.ShaclTargetSubjectsOf;
import org.eclipse.lyo.validation.shacl.annotations.ShaclUniqueLang;

/**
 * Factory for creating Shacl shapes form the Abstract Resource Classes.
 * <p>
 * It reads the Shacl constraints or oslc constraints in the form of annotations and
 * populate the ShaclShape instance and property instances.
 * <p>
 * It checks whether the ShaclAnnotations are available in Abstract Resource Class.
 * If it does, then it reads the Shacl Annotations for populating ShaclShape instance.
 * Otherwise,it reads the oslc annotations to create ShaclShape instance.
 *
 * @author Yash Khatri
 * @version $version-stub$
 * @since 2.3.0
 */
public final class ShaclShapeFactory extends ResourceShapeFactory {

    private ShaclShapeFactory() {
        super();
    }

    /**
     * Create a SHACL shape from the {@link org.eclipse.lyo.validation.shacl.annotations} on the
     * resource declaration.
     *
     * @param resourceClass Resource class that is annotated with shape annotations.
     *
     * @return an instance of {@link Shape} shape
     */
    public static Shape createShaclShape(final Class<?> resourceClass)
            throws OslcCoreApplicationException, URISyntaxException, ParseException {
        final HashSet<Class<?>> verifiedClasses = new HashSet<Class<?>>();
        verifiedClasses.add(resourceClass);

        return createShaclShape(resourceClass, verifiedClasses);
    }

    private static Shape createShaclShape(final Class<?> resourceClass,
            final Set<Class<?>> verifiedClasses)
            throws OslcCoreApplicationException, URISyntaxException, ParseException {
        final OslcResourceShape resourceShapeAnnotation = resourceClass.getAnnotation(
                OslcResourceShape.class);
        if (resourceShapeAnnotation == null) {
            throw new OslcCoreMissingAnnotationException(resourceClass, OslcResourceShape.class);
        }

        OslcNamespace oslcNamespace = resourceClass.getAnnotation(OslcNamespace.class);
        OslcName oslcName = resourceClass.getAnnotation(OslcName.class);

        final URI about = new URI(oslcNamespace.value() + oslcName.value());
        final Shape shaclShape = new Shape(about);

        populateFromClassLevelAnnotations(shaclShape, resourceClass);

        final Set<String> propertyDefinitions = new HashSet<String>();

        //Create Properties. Also check if shacl annotations are available or not.
        createProperties(resourceClass, verifiedClasses, shaclShape, propertyDefinitions, true);

        //If shacl annotations are available. No need to populate property instance from oslc
        // annotations.
        if (!shaclShape.isReadShaclAnnotations()) {
            shaclShape.setShaclProperties(null);
            shaclShape.setTargetClass(new URI(resourceClass.getAnnotation(OslcNamespace.class).value()
                    + resourceClass.getAnnotation(OslcName.class).value()));
            propertyDefinitions.clear();
            createProperties(resourceClass, verifiedClasses, shaclShape, propertyDefinitions,
                    false);
        }

        //resetting it to false again.
        shaclShape.setReadShaclAnnotations(false);
        return shaclShape;
    }

    /**
     * Populates the class level annotations in the {@link Shape} instance.
     */
    private static void populateFromClassLevelAnnotations(Shape shaclShape,
            final Class<?> resourceClass) throws URISyntaxException {

        //Target Constraints Start
        final ShaclTargetNode shaclTargetNode = resourceClass.getAnnotation(ShaclTargetNode.class);
        if (shaclTargetNode != null) {
            shaclShape.setTargetNode(new URI(shaclTargetNode.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclTargetObjectsOf shaclTargetObjectsOf = resourceClass.getAnnotation(
                ShaclTargetObjectsOf.class);
        if (shaclTargetObjectsOf != null) {
            shaclShape.setTargetObjectsOf(new URI(shaclTargetObjectsOf.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclTargetSubjectsOf shaclTargetSubjectsOf = resourceClass.getAnnotation(
                ShaclTargetSubjectsOf.class);
        if (shaclTargetSubjectsOf != null) {
            shaclShape.setTargetSubjectsOf(new URI(shaclTargetSubjectsOf.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclTargetClass shaclTargetClass = resourceClass.getAnnotation(
                ShaclTargetClass.class);
        if (shaclTargetClass != null) {
            shaclShape.setTargetClass(new URI(shaclTargetClass.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        //Target Constraints End

        final RdfsIsDefinedBy isDefinedBy = resourceClass.getAnnotation(RdfsIsDefinedBy.class);
        if (isDefinedBy != null) {
            shaclShape.setIsDefinedBy(new URI(isDefinedBy.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final RdfsLabel label = resourceClass.getAnnotation(RdfsLabel.class);
        if (label != null) {
            shaclShape.setLabel(label.value());
            shaclShape.setReadShaclAnnotations(true);
        }

        final RDFType rdfTypeAnotation = resourceClass.getAnnotation(RDFType.class);
        if (rdfTypeAnotation != null) {
            shaclShape.setType(new URI(rdfTypeAnotation.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclClosed shaclClosed = resourceClass.getAnnotation(ShaclClosed.class);
        if (shaclClosed != null) {
            shaclShape.setClosed(shaclClosed.value());
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclIgnoredProperties shaclIgnoredProperties = resourceClass.getAnnotation(
                ShaclIgnoredProperties.class);
        if (shaclIgnoredProperties != null) {
            List<URI> ignoredPropertiesList = populateIgnoredProperties(shaclIgnoredProperties);
            shaclShape.setIgnoredProperties(ignoredPropertiesList);
            shaclShape.setReadShaclAnnotations(true);
        }
    }

    /**
     * @param shaclIgnoredProperties
     * @return
     * @throws URISyntaxException
     */
    private static List<URI> populateIgnoredProperties(
            final ShaclIgnoredProperties shaclIgnoredProperties) throws URISyntaxException {
        List<URI> ignoredPropertiesList = new ArrayList<URI>();
        for (String ignoredProperty : shaclIgnoredProperties.values()) {
            ignoredPropertiesList.add(new URI(ignoredProperty));
        }
        return ignoredPropertiesList;
    }

    /**
     * @throws OslcCoreMissingSetMethodException This is the entry method for creating properties.
     */
    private static void createProperties(final Class<?> resourceClass,
            final Set<Class<?>> verifiedClasses, final Shape shaclShape,
            final Set<String> propertyDefinitions, boolean chooseShacl)
            throws OslcCoreDuplicatePropertyDefinitionException, URISyntaxException,
            OslcCoreApplicationException, OslcCoreMissingSetMethodException, ParseException {

        for (final Method method : resourceClass.getMethods()) {
            if (isValidAccessorMethod(method, resourceClass, propertyDefinitions)) {
                final OslcPropertyDefinition propertyDefinitionAnnotation =
                        InheritedMethodAnnotationHelper
                        .getAnnotation(method, OslcPropertyDefinition.class);
                createPropertiesInternal(resourceClass, verifiedClasses, shaclShape,
                        propertyDefinitions, chooseShacl, method, propertyDefinitionAnnotation,
                        propertyDefinitionAnnotation.value());
            }
        }
    }

    /**
     * @param resourceClass
     * @param propertyDefinitions
     * @throws SecurityException
     * @throws OslcCoreDuplicatePropertyDefinitionException
     */
    private static boolean isValidAccessorMethod(Method method, final Class<?> resourceClass,
            final Set<String> propertyDefinitions)
            throws SecurityException, OslcCoreDuplicatePropertyDefinitionException {

        if (method.getParameterTypes().length == 0) {
            final String methodName = method.getName();
            final int methodNameLength = methodName.length();
            if ((((methodName.startsWith(
                    METHOD_NAME_START_GET)) && (methodNameLength > METHOD_NAME_START_GET_LENGTH))
                    || ((methodName
                    .startsWith(
                            METHOD_NAME_START_IS)) && (methodNameLength >
                    METHOD_NAME_START_IS_LENGTH))) && !method
                    .getReturnType()
                    .equals(Void.TYPE)) {
                final OslcPropertyDefinition propertyDefinitionAnnotation =
                        InheritedMethodAnnotationHelper
                        .getAnnotation(method, OslcPropertyDefinition.class);
                if (propertyDefinitionAnnotation != null) {
                    final String propertyDefinition = propertyDefinitionAnnotation.value();
                    if (propertyDefinitions.contains(propertyDefinition)) {
                        throw new OslcCoreDuplicatePropertyDefinitionException(resourceClass,
                                propertyDefinitionAnnotation);
                    }

                    return true;
                }
            }
        }
        return false;
    }

    /**
     * @param resourceClass
     * @param verifiedClasses
     * @param shaclShape
     * @param propertyDefinitions
     * @param chooseShacl
     * @param method
     * @param propertyDefinitionAnnotation
     * @param propertyDefinition
     * @throws URISyntaxException
     * @throws OslcCoreApplicationException
     * @throws ParseException
     * @throws OslcCoreMissingSetMethodException
     */
    private static void createPropertiesInternal(final Class<?> resourceClass,
            final Set<Class<?>> verifiedClasses, final Shape shaclShape,
            final Set<String> propertyDefinitions, boolean chooseShacl, final Method method,
            final OslcPropertyDefinition propertyDefinitionAnnotation,
            final String propertyDefinition)
            throws URISyntaxException, OslcCoreApplicationException, ParseException,
            OslcCoreMissingSetMethodException {
        propertyDefinitions.add(propertyDefinition);
        if (chooseShacl) {
            final Property property = createPropertiesFromShaclAnnotations(resourceClass,
                    method, propertyDefinitionAnnotation, verifiedClasses, shaclShape);
            shaclShape.addProperty(property);
        } else {
            final Property property = createPropertiesFromOslcAnnotations(resourceClass,
                    method, propertyDefinitionAnnotation, verifiedClasses);
            shaclShape.addProperty(property);
        }
        validateSetMethodExists(resourceClass, method);
    }

    /**
     * @throws URISyntaxException It is a common method for creating properties.
     */
    @SuppressWarnings("rawtypes") // supress warning when casting Arrays.asList() to a Collection
    private static Class<?> createPropertyCommon(final Class<?> resourceClass, final Method method,
            final OslcPropertyDefinition propertyDefinitionAnnotation,
            final Set<Class<?>> verifiedClasses)
            throws OslcCoreApplicationException, URISyntaxException {
        final String name;

        final OslcName nameAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method,
                OslcName.class);
        if (nameAnnotation != null) {
            name = nameAnnotation.value();
        } else {
            name = getDefaultPropertyName(method);
        }

        final String propertyDefinition = propertyDefinitionAnnotation.value();

        if (!propertyDefinition.endsWith(name)) {
            throw new OslcCoreInvalidPropertyDefinitionException(resourceClass, method,
                    propertyDefinitionAnnotation);
        }

        final Class<?> returnType = method.getReturnType();

        Class<?> componentType = getComponentType(resourceClass, method, returnType);

        // Reified resources are a special case.
        if (IReifiedResource.class.isAssignableFrom(componentType)) {
            final Type genericType = componentType.getGenericSuperclass();

            if (genericType instanceof ParameterizedType) {
                final ParameterizedType parameterizedType = (ParameterizedType) genericType;
                final Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
                if (actualTypeArguments.length == 1) {
                    final Type actualTypeArgument = actualTypeArguments[0];
                    if (actualTypeArgument instanceof Class) {
                        componentType = (Class<?>) actualTypeArgument;
                    }
                }
            }
        }

        return componentType;
    }

    /**
     * @throws OslcCoreApplicationException This method reads the OSLC annotations for populating
     *                                      property instances.
     */
    private static Property createPropertiesFromOslcAnnotations(Class<?> resourceClass,
            Method method, OslcPropertyDefinition propertyDefinitionAnnotation,
            Set<Class<?>> verifiedClasses)
            throws URISyntaxException, OslcCoreApplicationException, ParseException {

        Class<?> componentType = createPropertyCommon(resourceClass, method,
                propertyDefinitionAnnotation, verifiedClasses);

        final Property property = new Property();
        property.setPath(new URI(propertyDefinitionAnnotation.value()));

        //Setting Value Type
        ValueType valueType = null;
        final OslcValueType valueTypeAnnotation = InheritedMethodAnnotationHelper.getAnnotation(
                method, OslcValueType.class);
        if (valueTypeAnnotation != null) {
            valueType = valueTypeAnnotation.value();
            validateUserSpecifiedValueType(resourceClass, method, valueType, componentType);
            property.setDataType(new URI(valueType.toString()));

            final OslcAllowedValue oslcAllowedValue = InheritedMethodAnnotationHelper.getAnnotation(
                    method, OslcAllowedValue.class);
            if (oslcAllowedValue != null) {
                property.setIn(oslcAllowedValue.value());
            }

            final OslcAllowedValues oslcAllowedValues = InheritedMethodAnnotationHelper
                    .getAnnotation(
                    method, OslcAllowedValues.class);
            if (oslcAllowedValues != null) {
                property.addIn(oslcAllowedValues);
            }

        }

        //Cardinality Constraint Components Start
        final OslcOccurs oslcOccurs = InheritedMethodAnnotationHelper.getAnnotation(method,
                OslcOccurs.class);
        if (oslcOccurs != null && oslcOccurs.value().equals(Occurs.ExactlyOne)) {
            property.setMaxCount(new BigInteger("1"));
            property.setMinCount(new BigInteger("1"));
        }

        if (oslcOccurs != null && oslcOccurs.value().equals(Occurs.OneOrMany)) {
            property.setMinCount(new BigInteger("1"));
        }

        if (oslcOccurs != null && oslcOccurs.value().equals(Occurs.ZeroOrMany)) {
            property.setMinCount(new BigInteger("0"));
        }

        if (oslcOccurs != null && oslcOccurs.value().equals(Occurs.ZeroOrOne)) {
            property.setMinCount(new BigInteger("0"));
            property.setMaxCount(new BigInteger("1"));
        }
        //Cardinality Constraint Components End

        //String Based Constraints Start
        final OslcMaxSize oslcMaxSize = InheritedMethodAnnotationHelper.getAnnotation(method,
                OslcMaxSize.class);
        if (oslcMaxSize != null) {
            property.setMaxLength(new BigInteger(String.valueOf(oslcMaxSize.value())));
        }
        //String Based Constraints End

        final OslcName oslcName = InheritedMethodAnnotationHelper.getAnnotation(method,
                OslcName.class);
        if (oslcName != null) {
            property.setName(oslcName.value());
        }

        final OslcDescription oslcDescription = InheritedMethodAnnotationHelper.getAnnotation(
                method, OslcDescription.class);
        if (oslcDescription != null) {
            property.setDescription(oslcDescription.value());
        }

        final OslcValueShape oslcValueShape = InheritedMethodAnnotationHelper.getAnnotation(method,
                OslcValueShape.class);
        if (oslcValueShape != null) {
            property.setNode(new URI(oslcValueShape.value()));
        }

        final OslcRange oslcRange = InheritedMethodAnnotationHelper.getAnnotation(method,
                OslcRange.class);
        if (oslcRange != null) {
            property.setClassType(
                    new URI(oslcRange.value()[0]));   //have multiple values check this..
        }

        //Other Constraint Components

        return property;
    }

    /**
     * Reads shacl annotations for populating property instances.
     */
    private static Property createPropertiesFromShaclAnnotations(final Class<?> resourceClass,
            final Method method, final OslcPropertyDefinition propertyDefinitionAnnotation,
            final Set<Class<?>> verifiedClasses, Shape shaclShape)
            throws URISyntaxException, OslcCoreApplicationException, ParseException {

        Class<?> componentType = createPropertyCommon(resourceClass, method,
                propertyDefinitionAnnotation, verifiedClasses);

        final Property property = new Property();
        property.setPath(new URI(propertyDefinitionAnnotation.value()));

        //Setting Value Type
        ValueType dataType = null;
        final ShaclDataType dataTypeAnnotation = InheritedMethodAnnotationHelper.getAnnotation(
                method, ShaclDataType.class);
        if (dataTypeAnnotation != null) {
            dataType = dataTypeAnnotation.value();
            property.setDataType(new URI(dataType.toString()));
            shaclShape.setReadShaclAnnotations(true);
        }
        // Other Constraint Components
        final ShaclIn inAnnotation = InheritedMethodAnnotationHelper.getAnnotation(method, ShaclIn.class);
        if (inAnnotation != null) {
            property.setIn(inAnnotation.value());
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclDescription shaclDescription = InheritedMethodAnnotationHelper.getAnnotation(
                method, ShaclDescription.class);
        if (shaclDescription != null) {
            property.setDescription(shaclDescription.value());
        }

        final ShaclDataType dataTypeAnotation = InheritedMethodAnnotationHelper.getAnnotation(
                method, ShaclDataType.class);
        if (dataTypeAnotation != null) {
            property.setDataType(new URI(dataTypeAnotation.value().toString()));
            shaclShape.setReadShaclAnnotations(true);
        }

        //Cardinality Constraint Components Start
        final ShaclMaxCount maxCountAnnotation = InheritedMethodAnnotationHelper.getAnnotation(
                method, ShaclMaxCount.class);
        if (maxCountAnnotation != null) {
            property.setMaxCount(new BigInteger(String.valueOf(maxCountAnnotation.value())));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclMinCount minCountAnnotation = InheritedMethodAnnotationHelper.getAnnotation(
                method, ShaclMinCount.class);
        if (minCountAnnotation != null) {
            property.setMinCount(new BigInteger(String.valueOf(minCountAnnotation.value())));
            shaclShape.setReadShaclAnnotations(true);
        }
        //Cardinality Constraint Components End

        //Value Range Constraint Components Start
        final ShaclMinExclusive minExclusiveAnnotation = InheritedMethodAnnotationHelper
                .getAnnotation(
                method, ShaclMinExclusive.class);
        if (minExclusiveAnnotation != null) {
            property.setMinExclusive(
                    new BigInteger(String.valueOf(minExclusiveAnnotation.value())));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclMaxExclusive maxExclusiveAnnotation = InheritedMethodAnnotationHelper
                .getAnnotation(
                method, ShaclMaxExclusive.class);
        if (maxExclusiveAnnotation != null) {
            property.setMaxExclusive(
                    new BigInteger(String.valueOf(maxExclusiveAnnotation.value())));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclMinInclusive minInclusiveAnnotation = InheritedMethodAnnotationHelper
                .getAnnotation(
                method, ShaclMinInclusive.class);
        if (minInclusiveAnnotation != null) {
            property.setMinInclusive(
                    new BigInteger(String.valueOf(minInclusiveAnnotation.value())));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclMaxInclusive maxInclusiveAnnotation = InheritedMethodAnnotationHelper
                .getAnnotation(
                method, ShaclMaxInclusive.class);
        if (maxInclusiveAnnotation != null) {
            property.setMaxInclusive(
                    new BigInteger(String.valueOf(maxInclusiveAnnotation.value())));
            shaclShape.setReadShaclAnnotations(true);
        }
        //Value Range Constraint Components End

        final ShaclClassType shaclClass = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclClassType.class);
        if (shaclClass != null) {
            property.setClassType(new URI(shaclClass.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclNode shaclNode = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclNode.class);
        if (shaclNode != null) {
            property.setNode(new URI(shaclNode.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        //Non Validating Constraints Start
        final ShaclName shaclName = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclName.class);
        if (shaclName != null) {
            property.setName(shaclName.value());
            shaclShape.setReadShaclAnnotations(true);
        }

        final OslcDescription oslcDescription = InheritedMethodAnnotationHelper.getAnnotation(
                method, OslcDescription.class);
        if (oslcDescription != null) {
            property.setDescription(oslcDescription.value());
        }

        final ShaclGroup shaclGroup = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclGroup.class);
        if (shaclGroup != null) {
            property.setGroup(new URI(shaclGroup.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclOrder shaclOrder = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclOrder.class);
        if (shaclOrder != null) {
            property.setOrder(new BigInteger(String.valueOf(shaclOrder.value())));
            shaclShape.setReadShaclAnnotations(true);
        }
        //Non Validating Constraints End

        //String Based Constraints Start
        final ShaclPattern shaclPattern = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclPattern.class);
        if (shaclPattern != null) {
            property.setPattern(shaclPattern.value());
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclUniqueLang shaclUniqueLang = InheritedMethodAnnotationHelper.getAnnotation(
                method, ShaclUniqueLang.class);
        if (shaclUniqueLang != null) {
            property.setUniqueLang(shaclUniqueLang.value());
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclLanguageIn shaclLanguageIn = InheritedMethodAnnotationHelper.getAnnotation(
                method, ShaclLanguageIn.class);
        if (shaclLanguageIn != null) {
            property.setLanguageIn(shaclLanguageIn.value());
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclMinLength shaclMinLength = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclMinLength.class);
        if (shaclMinLength != null) {
            property.setMinLength(new BigInteger(String.valueOf(shaclMinLength.value())));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclMaxLength shaclMaxLength = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclMaxLength.class);
        if (shaclMaxLength != null) {
            property.setMaxLength(new BigInteger(String.valueOf(shaclMaxLength.value())));
            shaclShape.setReadShaclAnnotations(true);
        }
        //String Based Constraints End

        final ShaclHasValue shaclHasValue = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclHasValue.class);
        if (shaclHasValue != null) {
            property.setHasValue(new URI(shaclHasValue.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        //Property Pair Constraint Components Start
        final ShaclEquals shaclEquals = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclEquals.class);
        if (shaclEquals != null) {
            property.setEquals(new URI(shaclEquals.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclDisjoint shaclDisjoint = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclDisjoint.class);
        if (shaclDisjoint != null) {
            property.setDisjoint(new URI(shaclDisjoint.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclLessThan shaclLessThan = InheritedMethodAnnotationHelper.getAnnotation(method,
                ShaclLessThan.class);
        if (shaclLessThan != null) {
            property.setLessThan(new URI(shaclLessThan.value()));
            shaclShape.setReadShaclAnnotations(true);
        }

        final ShaclLessThanOrEquals shaclLessThanOrEquals = InheritedMethodAnnotationHelper
                .getAnnotation(
                method, ShaclLessThanOrEquals.class);
        if (shaclLessThanOrEquals != null) {
            property.setLessThanOrEquals(new URI(shaclLessThanOrEquals.value()));
            shaclShape.setReadShaclAnnotations(true);
        }
        return property;
    }
}
