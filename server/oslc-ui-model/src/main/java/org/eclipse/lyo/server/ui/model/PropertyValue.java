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
package org.eclipse.lyo.server.ui.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "data",
    "representAsList",
    "representationType"
})
public class PropertyValue {

    /**
     *
     * (Required)
     *
     */
    @JsonProperty("data")
    private Object data;
    @JsonProperty("representAsList")
    private Boolean representAsList;
    /**
     *
     * (Required)
     *
     */
    @JsonProperty("representationType")
    private PropertyDefintion.RepresentationType representationType;

    /**
     *
     * (Required)
     *
     */
    @JsonProperty("data")
    public Object getData() {
        return data;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonProperty("data")
    public void setData(Object data) {
        this.data = data;
    }

    @JsonProperty("representAsList")
    public Boolean getRepresentAsList() {
        return representAsList;
    }

    @JsonProperty("representAsList")
    public void setRepresentAsList(Boolean representAsList) {
        this.representAsList = representAsList;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonProperty("representationType")
    public PropertyDefintion.RepresentationType getRepresentationType() {
        return representationType;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonProperty("representationType")
    public void setRepresentationType(PropertyDefintion.RepresentationType representationType) {
        this.representationType = representationType;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(PropertyValue.class.getName()).append('@').append(Integer.toHexString(System.identityHashCode(this))).append('[');
        sb.append("data");
        sb.append('=');
        sb.append(((this.data == null)?"<null>":this.data));
        sb.append(',');
        sb.append("representAsList");
        sb.append('=');
        sb.append(((this.representAsList == null)?"<null>":this.representAsList));
        sb.append(',');
        sb.append("representationType");
        sb.append('=');
        sb.append(((this.representationType == null)?"<null>":this.representationType));
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
        result = ((result* 31)+((this.representationType == null)? 0 :this.representationType.hashCode()));
        result = ((result* 31)+((this.data == null)? 0 :this.data.hashCode()));
        result = ((result* 31)+((this.representAsList == null)? 0 :this.representAsList.hashCode()));
        return result;
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if ((other instanceof PropertyValue) == false) {
            return false;
        }
        PropertyValue rhs = ((PropertyValue) other);
        return ((((this.representationType == rhs.representationType)||((this.representationType!= null)&&this.representationType.equals(rhs.representationType)))&&((this.data == rhs.data)||((this.data!= null)&&this.data.equals(rhs.data))))&&((this.representAsList == rhs.representAsList)||((this.representAsList!= null)&&this.representAsList.equals(rhs.representAsList))));
    }

}
