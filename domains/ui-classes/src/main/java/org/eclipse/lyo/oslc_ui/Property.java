/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
package org.eclipse.lyo.oslc_ui;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "propertyDefintion",
    "propertyValue"
})
public class Property {

    /**
     * 
     * (Required)
     * 
     */
    @JsonProperty("propertyDefintion")
    private PropertyDefintion propertyDefintion;
    /**
     * 
     * (Required)
     * 
     */
    @JsonProperty("propertyValue")
    private PropertyValue propertyValue;

    /**
     * 
     * (Required)
     * 
     */
    @JsonProperty("propertyDefintion")
    public PropertyDefintion getPropertyDefintion() {
        return propertyDefintion;
    }

    /**
     * 
     * (Required)
     * 
     */
    @JsonProperty("propertyDefintion")
    public void setPropertyDefintion(PropertyDefintion propertyDefintion) {
        this.propertyDefintion = propertyDefintion;
    }

    /**
     * 
     * (Required)
     * 
     */
    @JsonProperty("propertyValue")
    public PropertyValue getPropertyValue() {
        return propertyValue;
    }

    /**
     * 
     * (Required)
     * 
     */
    @JsonProperty("propertyValue")
    public void setPropertyValue(PropertyValue propertyValue) {
        this.propertyValue = propertyValue;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(Property.class.getName()).append('@').append(Integer.toHexString(System.identityHashCode(this))).append('[');
        sb.append("propertyDefintion");
        sb.append('=');
        sb.append(((this.propertyDefintion == null)?"<null>":this.propertyDefintion));
        sb.append(',');
        sb.append("propertyValue");
        sb.append('=');
        sb.append(((this.propertyValue == null)?"<null>":this.propertyValue));
        sb.append(',');
        if (sb.charAt((sb.length()- 1)) == ',') {
            sb.setCharAt((sb.length()- 1), ']');
        } else {
            sb.append(']');
        }
        return sb.toString();
    }

    @Override
    public int hashCode() {
        int result = 1;
        result = ((result* 31)+((this.propertyValue == null)? 0 :this.propertyValue.hashCode()));
        result = ((result* 31)+((this.propertyDefintion == null)? 0 :this.propertyDefintion.hashCode()));
        return result;
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if ((other instanceof Property) == false) {
            return false;
        }
        Property rhs = ((Property) other);
        return (((this.propertyValue == rhs.propertyValue)||((this.propertyValue!= null)&&this.propertyValue.equals(rhs.propertyValue)))&&((this.propertyDefintion == rhs.propertyDefintion)||((this.propertyDefintion!= null)&&this.propertyDefintion.equals(rhs.propertyDefintion))));
    }

}
