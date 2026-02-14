/*
 * Copyright (c) 2022 Contributors to the Eclipse Foundation
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
package org.eclipse.lyo.oslc4j.provider.json4j.internal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Iterator;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonException;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

/**
 * This class aims to wrap Jakarta JsonArray to be usable in JsonHelper as before.
 * Unlike Wink's JSONArray, Jakarta ones are immutable, which makes us work with JsonArrayBuilder before building the object.
 */
public class JSONArray implements Iterable<JsonValue> {

    private JsonArrayBuilder builder;

    public JSONArray() {
        this.builder = Json.createArrayBuilder();
    }

    public JSONArray(JsonArray array) {
        this.builder = Json.createArrayBuilder(array);
    }

    /*
     * Builds the jakarta.json array.
     * As this method is also internally called for implementing read-only operations,
     * the underlying builder is reset with the built array instead of being set to empty,
     * so that additional values can still be added later on.
     */
    public JsonArray build() {
        JsonArray jakartaObject = builder.build();
        this.builder = Json.createArrayBuilder(jakartaObject);
        return jakartaObject;
    }

    @Override
    public String toString() {
        return build().toString();
    }

    public boolean isEmpty() {
        return build().isEmpty();
    }

    public int size() {
        return build().size();
    }

    public JsonValue get(int index) {
        return build().get(index);
    }

    public JSONObject getJSONObject(int index) {
        JsonArray array = build();
        JsonValue value = array.get(index);
        if (value != null) {
            return new JSONObject((JsonObject) value);
        } else {
            throw new JsonException("The value for index: [" + index + "] was null.  Object required.");
        }
    }

    public void add(Object object) {
        if (object instanceof String) {
            this.builder.add((String) object);
        } else if (object instanceof JSONObject) {
            this.builder.add(((JSONObject) object).build());
        } else if (object instanceof JSONArray) {
            this.builder.add(((JSONArray) object).build());
        } else if (object instanceof JsonObjectBuilder) {
            this.builder.add((JsonObjectBuilder) object);
        } else if (object instanceof JsonArrayBuilder) {
            this.builder.add((JsonArrayBuilder) object);
        } else if (object instanceof JsonValue) {
            this.builder.add((JsonValue) object);
        } else if (object instanceof Boolean) {
            this.builder.add((Boolean) object);
        } else if (object instanceof BigDecimal) {
            this.builder.add((BigDecimal) object);
        } else if (object instanceof BigInteger) {
            this.builder.add((BigInteger) object);
        } else if (object instanceof Long) {
            this.builder.add((Long) object);
        } else if (object instanceof Double) {
            this.builder.add((Double) object);
        } else if (object instanceof Integer) {
            this.builder.add((Integer) object);
        } else {
            throw new IllegalArgumentException("Element cannot be added to JsonArray.");
        }
    }

    @Override
    public Iterator<JsonValue> iterator() {
        return build().iterator();
    }

}
