/*******************************************************************************
 * Copyright (c) 2012, 2014 IBM Corporation.
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
 *
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *     Samuel Padgett       - support allowed and default values other than string
 *     Samuel Padgett       - copy from lyo.core and modify specifically for automation plans
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.resources;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.lyo.oslc4j.core.annotation.OslcAllowedValue;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.Representation;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

/**
 * Identical to {@link org.eclipse.lyo.oslc4j.core.model.Property}, except that
 * it does not have an oslc:propertyDefinition field. This class is intended to
 * only be used for OSLC automation plans.
 */
@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Property Resource Shape", describes = OslcConstants.TYPE_PROPERTY)
public final class Property extends AbstractResource implements Comparable<Property> {
	private static final QName PROPERTY_ALLOWED_VALUE = new QName(OslcConstants.OSLC_CORE_NAMESPACE, "allowedValue");
	private static final QName PROPERTY_DEFAULT_VALUE = new QName(OslcConstants.OSLC_CORE_NAMESPACE, "defaultValue");
	private final List<URI> range = new ArrayList<URI>();

    private URI allowedValuesRef;
	private String description;
	private Boolean hidden;
	private Integer maxSize;
	private Boolean memberProperty;
	private String name;
	private Occurs occurs;
	private Boolean readOnly;
	private Representation representation;
	private String title;
	private URI valueShape;
	private ValueType valueType;

	public Property() {
	    super();
	}

	public Property(final String name,
			final Occurs occurs,
			final ValueType valueType) {
		this();

		this.name = name;
		this.occurs = occurs;
		this.valueType = valueType;
	}

	public void addRange(final URI range) {
		this.range.add(range);
	}

	@Override
	public int compareTo(final Property o) {
		return name.compareTo(o.getName());
	}

	@OslcDescription("Resource with allowed values for the property being defined")
	@OslcName("allowedValues")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "allowedValues")
	@OslcRange(OslcConstants.TYPE_ALLOWED_VALUES)
	@OslcReadOnly
    @OslcTitle("Allowed Value Reference")
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + OslcConstants.PATH_ALLOWED_VALUES)
	public URI getAllowedValuesRef() {
	    return allowedValuesRef;
	}

	public Object getDefaultValueObject() {
		return getExtendedProperties().get(PROPERTY_DEFAULT_VALUE);
	}

	/**
	 * @deprecated Use {@link #getDefaultValueObject()}, which supports types other than String
	 */
	@Deprecated
	public String getDefaultValue() {
		Object o = getExtendedProperties().get(PROPERTY_DEFAULT_VALUE);

		// Backwards compatibility: Only return a value if it's a string.
		return (o instanceof String) ? (String) o : null;
	}

	@OslcDescription("Description of the property. SHOULD include only content that is valid and suitable inside an XHTML <div> element")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "description")
	@OslcReadOnly
    @OslcTitle("Description")
    @OslcValueType(ValueType.XMLLiteral)
	public String getDescription() {
		return description;
	}

	@OslcDescription("For String properties only, specifies maximum characters allowed. If not set, then there is no maximum or maximum is specified elsewhere")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "maxSize")
	@OslcReadOnly
    @OslcTitle("Maximum Size")
	public Integer getMaxSize() {
		return maxSize;
	}

	@OslcDescription("Name of property being defined, i.e. second part of property's Prefixed Name")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "name")
	@OslcReadOnly
	@OslcTitle("Name")
    public String getName() {
		return name;
	}

	@OslcAllowedValue({OslcConstants.OSLC_CORE_NAMESPACE + "Exactly-one",
                       OslcConstants.OSLC_CORE_NAMESPACE + "Zero-or-one",
                       OslcConstants.OSLC_CORE_NAMESPACE + "Zero-or-many",
                       OslcConstants.OSLC_CORE_NAMESPACE + "One-or-many"})
	@OslcDescription("MUST be either http://open-services.net/ns/core#Exactly-one, http://open-services.net/ns/core#Zero-or-one, http://open-services.net/ns/core#Zero-or-many or http://open-services.net/ns/core#One-or-many")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "occurs")
	@OslcReadOnly
    @OslcTitle("Occurs")
	public URI getOccurs() {
	    if (occurs != null) {
	        try {
	            return new URI(occurs.toString());
	        } catch (final URISyntaxException exception) {
	            // This should never happen since we control the possible values of the Occurs enum.
	            throw new RuntimeException(exception);
	        }
	    }

	    return null;
	}

	@OslcDescription("For properties with a resource value-type, Providers MAY also specify the range of possible resource classes allowed, each specified by URI. The default range is http://open-services.net/ns/core#Any")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "range")
	@OslcReadOnly
    @OslcTitle("Ranges")
	public URI[] getRange() {
	    return range.toArray(new URI[range.size()]);
	}

	@OslcAllowedValue({OslcConstants.OSLC_CORE_NAMESPACE + "Reference",
	                   OslcConstants.OSLC_CORE_NAMESPACE + "Inline",
	                   OslcConstants.OSLC_CORE_NAMESPACE + "Either"})
    @OslcDescription("Should be http://open-services.net/ns/core#Reference, http://open-services.net/ns/core#Inline or http://open-services.net/ns/core#Either")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "representation")
	@OslcReadOnly
    @OslcTitle("Representation")
	public URI getRepresentation() {
	    if (representation != null) {
	        try {
	            return new URI(representation.toString());
	        } catch (final URISyntaxException exception) {
	            // This should never happen since we control the possible values of the Representation enum.
	            throw new RuntimeException(exception);
	        }
	    }

	    return null;
	}

	@OslcDescription("Title of the property. SHOULD include only content that is valid and suitable inside an XHTML <div> element")
	@OslcPropertyDefinition(OslcConstants.DCTERMS_NAMESPACE + "title")
	@OslcReadOnly
    @OslcTitle("Title")
	@OslcValueType(ValueType.XMLLiteral)
    public String getTitle() {
		return title;
	}

	@OslcDescription("if the value-type is a resource type, then Property MAY provide a shape value to indicate the Resource Shape that applies to the resource")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "valueShape")
	@OslcRange(OslcConstants.TYPE_RESOURCE_SHAPE)
	@OslcReadOnly
    @OslcTitle("Value Shape")
    public URI getValueShape() {
	    return valueShape;
	}

	@OslcAllowedValue({OslcConstants.XML_NAMESPACE + "boolean",
	                   OslcConstants.XML_NAMESPACE + "dateTime",
	                   OslcConstants.XML_NAMESPACE + "decimal",
	                   OslcConstants.XML_NAMESPACE + "double",
	                   OslcConstants.XML_NAMESPACE + "float",
	                   OslcConstants.XML_NAMESPACE + "integer",
	                   OslcConstants.XML_NAMESPACE + "string",
	                   OslcConstants.RDF_NAMESPACE + "XMLLiteral",
	                   OslcConstants.OSLC_CORE_NAMESPACE + "Resource",
	                   OslcConstants.OSLC_CORE_NAMESPACE + "LocalResource",
	                   OslcConstants.OSLC_CORE_NAMESPACE + "AnyResource"})
    @OslcDescription("See list of allowed values for oslc:valueType")
    @OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "valueType")
	@OslcReadOnly
    @OslcTitle("Value Type")
	public URI getValueType() {
	    if (valueType != null) {
	        try {
	            return new URI(valueType.toString());
	        } catch (final URISyntaxException exception) {
                // This should never happen since we control the possible values of the ValueType enum.
                throw new RuntimeException(exception);
            }
	    }

	    return null;
	}

	@OslcDescription("A hint that indicates that property MAY be hidden when presented in a user interface")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "hidden")
	@OslcReadOnly
    @OslcTitle("Hidden")
	public Boolean isHidden() {
		return hidden;
	}

	@OslcDescription("If set to true, this indicates that the property is a membership property, as described in the Query Syntax Specification: Member List Patterns. This is useful when the resource whose shape is being defined is viewed as a container of other resources. For example, look at the last example in Appendix B's RDF/XML Representation Examples: Specifying the shape of a query result, where blog:comment is defined as a membership property and comment that matches the query is returned as value of that property.")
	@OslcName("isMemberProperty")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "isMemberProperty")
	@OslcReadOnly
    @OslcTitle("Is Member Property")
	public Boolean isMemberProperty() {
		return memberProperty;
	}

	@OslcDescription("true if the property is read-only. If not set, or set to false, then the property is writable. Providers SHOULD declare a property read-only when changes to the value of that property will not be accepted on PUT. Consumers should note that the converse does not apply: Providers MAY reject a change to the value of a writable property.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "readOnly")
	@OslcReadOnly
    @OslcTitle("Read Only")
	public Boolean isReadOnly() {
		return readOnly;
	}

	public void setAllowedValuesRef(final URI allowedValuesRef) {
	    if (allowedValuesRef != null) {
	        this.allowedValuesRef = allowedValuesRef;
	    } else {
	        this.allowedValuesRef = null;
	    }
	}

	public void setDefaultValue(final Object defaultValue) {
		if (defaultValue == null) {
			getExtendedProperties().remove(PROPERTY_DEFAULT_VALUE);
		} else {
			getExtendedProperties().put(PROPERTY_DEFAULT_VALUE, defaultValue);
		}
	}

	public void setDescription(final String description) {
		this.description = description;
	}

	public void setHidden(final Boolean hidden) {
		this.hidden = hidden;
	}

	public void setMaxSize(final Integer maxSize) {
		this.maxSize = maxSize;
	}

	public void setMemberProperty(final Boolean memberProperty) {
		this.memberProperty = memberProperty;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setOccurs(final Occurs occurs) {
	    this.occurs = occurs;
	}

	public void setOccurs(final URI occurs) {
	    if (occurs != null) {
	        this.occurs = Occurs.fromString(occurs.toString());
	    } else {
	        this.occurs = null;
	    }
	}

	public void setRange(final URI[] ranges) {
	    this.range.clear();
	    if (ranges != null) {
	        this.range.addAll(Arrays.asList(ranges));
	    }
	}

	public void setReadOnly(final Boolean readOnly) {
		this.readOnly = readOnly;
	}

	public void setRepresentation(final Representation representation) {
	    this.representation = representation;
	}

	public void setRepresentation(final URI representation) {
	    if (representation != null) {
	        this.representation = Representation.fromString(representation.toString());
	    } else {
	        this.representation = null;
	    }
	}

	public void setTitle(final String title) {
		this.title = title;
	}

	public void setValueShape(final URI valueShape) {
	    this.valueShape = valueShape;
	}

	public void setValueType(final ValueType valueType) {
	    this.valueType = valueType;
	}

	public void setValueType(final URI valueType) {
	    if (valueType != null) {
	        this.valueType = ValueType.fromString(valueType.toString());
	    } else {
	        this.valueType = null;
	    }
	}

    public Collection<?> getAllowedValuesCollection() {
        Collection<?> allowedValues = (Collection<?>) getExtendedProperties().get(PROPERTY_ALLOWED_VALUE);
		if (allowedValues == null) {
			return Collections.emptyList();
		}

		return allowedValues;
	}

	public void setAllowedValuesCollection(final Collection<?> values) {
		if (values == null) {
			getExtendedProperties().remove(PROPERTY_ALLOWED_VALUE);
		} else {
			getExtendedProperties().put(PROPERTY_ALLOWED_VALUE, values);
		}
	}

	/**
	 * @deprecated Use {@link #setAllowedValuesCollection(Collection)}, which allows for values other than String
	 */
	@Deprecated
	public void addAllowedValue(final String allowedValue) {
		ArrayList<Object> newValues = new ArrayList<Object>();
		@SuppressWarnings("unchecked")
        Collection<Object> allowedValues = (Collection<Object>) getExtendedProperties().get(PROPERTY_ALLOWED_VALUE);
		if (allowedValues != null) {
			newValues.addAll(allowedValues);
		}

		newValues.add(allowedValue);
		setAllowedValuesCollection(newValues);
	}

	/**
	 * @deprecated Use {@link #getAllowedValuesCollection()}, which allows for values other than String
	 */
	@Deprecated
    public String[] getAllowedValues() {
		// Be compatible with the old behavior and only include String values.
		ArrayList<String> stringValues = new ArrayList<String>();
		@SuppressWarnings("unchecked")
        Collection<Object> values = (Collection<Object>) getExtendedProperties().get(PROPERTY_ALLOWED_VALUE);
		if (values == null) {
			return new String[]{};
		}

		for (Object o : values) {
			if (o instanceof String) {
				stringValues.add((String) o);
			}
		}

		return stringValues.toArray(new String[stringValues.size()]);
    }

	/**
	 * @deprecated Use {@link #setAllowedValuesCollection(Collection)}, which allows for values other than String
	 */
	@Deprecated
	public void setAllowedValues(final String[] allowedValues) {
		getExtendedProperties().put(PROPERTY_ALLOWED_VALUE, allowedValues);
	}

}
