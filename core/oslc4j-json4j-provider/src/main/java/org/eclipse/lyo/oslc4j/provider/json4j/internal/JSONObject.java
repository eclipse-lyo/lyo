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
import java.util.Map.Entry;
import java.util.Set;

import jakarta.json.Json;
import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonException;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonValue;

/**
 * This class aims to wrap Jakarta JsonObject to be usable in JsonHelper as before.
 * Unlike Wink's JSONObject, Jakarta ones are immutable, which makes us work with JsonObjectBuilder before building the object.
 */
public class JSONObject {

    private JsonObjectBuilder builder;

    public JSONObject() {
        this.builder = Json.createObjectBuilder();
    }

    public JSONObject(JsonObject object) {
        this.builder = Json.createObjectBuilder(object);
    }

    /*
     * Builds the jakarta.json object.
     * As this method is also internally called for implementing read-only operations,
     * the underlying builder is reset with the built object instead of being set to empty,
     * so that additional properties can still be added later on.
     */
    public JsonObject build() {
        JsonObject jakartaObject = builder.build();
        this.builder = Json.createObjectBuilder(jakartaObject);
        return jakartaObject;
    }

    @Override
    public String toString() {
        return build().toString();
    }

    public int size() {
        return build().size();
    }

    public Object opt(String key) {
        return build().containsKey(key) ? toValue(build().get(key)) : null;
    }

    public Object get(String key) {
        JsonObject object = build();
        if (object.containsKey(key)) {
            return toValue(object.get(key));
        } else {
            throw new JsonException("The key [" + key + "] was not in the map.");
        }
    }

    public static Object toValue(JsonValue value) {
        if (value == null) return null;
        switch (value.getValueType()) {
            case STRING:
                return ((jakarta.json.JsonString) value).getString();
            case NUMBER:
                return ((jakarta.json.JsonNumber) value).numberValue();
            case TRUE:
                return Boolean.TRUE;
            case FALSE:
                return Boolean.FALSE;
            case NULL:
                return null;
            case OBJECT:
                return new JSONObject((JsonObject) value);
            case ARRAY:
                return new JSONArray((jakarta.json.JsonArray) value);
            default:
                return value;
        }
    }

    public String optString(String key) {
        JsonObject object = build();
        return object.getString(key, null);
    }

    public String getString(String key) {
        JsonObject object = build();
        if (object.containsKey(key)) {
            return object.getString(key);
        } else {
            throw new JsonException("The value for key: [" + key + "] was null.  Object required.");
        }
    }

    public JSONArray optJSONArray(String key) {
        JsonObject object = build();
        return object.containsKey(key) ? new JSONArray(object.getJsonArray(key)) : null;
    }

    public JSONObject optJSONObject(String key) {
        JsonObject object = build();
        return object.containsKey(key) ? new JSONObject(object.getJsonObject(key)) : null;
    }

    public JSONObject getJSONObject(String key) {
        JsonObject object = build();
        if (object.containsKey(key)) {
            return new JSONObject(object.getJsonObject(key));
        } else {
            throw new JsonException("The value for key: [" + key + "] was null.  Object required.");
        }
    }

    public JSONArray getJSONArray(String key) {
        JsonObject object = build();
        if (object.containsKey(key)) {
            return new JSONArray(object.getJsonArray(key));
        } else {
            throw new JsonException("The value for key: [" + key + "] was null.  Object required.");
        }
    }

    @SuppressWarnings("java:S3776") // Implementation is not that complex
    public void put(String key, Object object) {
        if (object == null) {
            this.builder.addNull(key);
        } else if (object instanceof String) {
            this.builder.add(key, (String) object);
        } else if (object instanceof JSONObject) {
            this.builder.add(key, ((JSONObject) object).build());
        } else if (object instanceof JSONArray) {
            this.builder.add(key, ((JSONArray) object).build());
        } else if (object instanceof JsonObjectBuilder) {
            this.builder.add(key, (JsonObjectBuilder) object);
        } else if (object instanceof JsonArrayBuilder) {
            this.builder.add(key, (JsonArrayBuilder) object);
        } else if (object instanceof JsonValue) {
            this.builder.add(key, (JsonValue) object);
        } else if (object instanceof Boolean) {
            this.builder.add(key, (Boolean) object);
        } else if (object instanceof BigDecimal) {
            this.builder.add(key, (BigDecimal) object);
        } else if (object instanceof BigInteger) {
            this.builder.add(key, (BigInteger) object);
        } else if (object instanceof Long) {
            this.builder.add(key, (Long) object);
        } else if (object instanceof Short) {
            this.builder.add(key, (Short) object);
        } else if (object instanceof Double) {
            this.builder.add(key, (Double) object);
        } else if (object instanceof Float) {
            this.builder.add(key, (Float) object);
        } else if (object instanceof Integer) {
            this.builder.add(key, (Integer) object);
        } else {
            throw new IllegalArgumentException("Unexpected value type for key '" + key + "': " + object.getClass().getName());
        }
    }

    public Set<Entry<String, JsonValue>> entrySet() {
        return build().entrySet();
    }

    public boolean containsKey(String key) {
        return build().containsKey(key);
    }

    public boolean has(String key) {
        return build().containsKey(key);
    }

    public boolean isEmpty() {
        return build().isEmpty();
    }

    public String write() {
        return toString();
    }

    public void write(java.io.Writer writer) {
        Json.createWriter(writer).write(build());
    }

}
