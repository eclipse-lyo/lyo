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

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import jakarta.json.bind.adapter.JsonbAdapter;
import jakarta.json.bind.annotation.JsonbProperty;
import jakarta.json.bind.annotation.JsonbPropertyOrder;
import jakarta.json.bind.annotation.JsonbTypeAdapter;

@JsonbPropertyOrder({
    "data",
    "representationType"
})
public class PropertyDefintion {

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("data")
    private Object data;
    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("representationType")
    private PropertyDefintion.RepresentationType representationType;

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("data")
    public Object getData() {
        return data;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("data")
    public void setData(Object data) {
        this.data = data;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("representationType")
    public PropertyDefintion.RepresentationType getRepresentationType() {
        return representationType;
    }

    /**
     *
     * (Required)
     *
     */
    @JsonbProperty("representationType")
    public void setRepresentationType(PropertyDefintion.RepresentationType representationType) {
        this.representationType = representationType;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(PropertyDefintion.class.getName()).append('@').append(Integer.toHexString(System.identityHashCode(this))).append('[');
        sb.append("data");
        sb.append('=');
        sb.append(((this.data == null)?"<null>":this.data));
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
        result = ((result* 31)+((this.data == null)? 0 :this.data.hashCode()));
        result = ((result* 31)+((this.representationType == null)? 0 :this.representationType.hashCode()));
        return result;
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if ((other instanceof PropertyDefintion) == false) {
            return false;
        }
        PropertyDefintion rhs = ((PropertyDefintion) other);
        return ((Objects.equals(this.data, rhs.data))&&(Objects.equals(this.representationType, rhs.representationType)));
    }

    @JsonbTypeAdapter(RepresentationTypeAdapter.class)
    public enum RepresentationType {

        TEXT("Text"),
        LINK("Link");
        private final String value;
        private final static Map<String, PropertyDefintion.RepresentationType> CONSTANTS = new HashMap<>();

        static {
            for (PropertyDefintion.RepresentationType c: values()) {
                CONSTANTS.put(c.value, c);
            }
        }

        private RepresentationType(String value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return this.value;
        }

        public String value() {
            return this.value;
        }

        public static PropertyDefintion.RepresentationType fromValue(String value) {
            PropertyDefintion.RepresentationType constant = CONSTANTS.get(value);
            if (constant == null) {
                throw new IllegalArgumentException(value);
            } else {
                return constant;
            }
        }

    }

    public static class RepresentationTypeAdapter implements JsonbAdapter<RepresentationType, String> {
        @Override
        public String adaptToJson(RepresentationType obj) throws Exception {
            return obj.value();
        }

        @Override
        public RepresentationType adaptFromJson(String obj) throws Exception {
            return RepresentationType.fromValue(obj);
        }
    }

}
