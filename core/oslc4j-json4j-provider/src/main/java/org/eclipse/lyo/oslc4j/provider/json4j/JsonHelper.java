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
package org.eclipse.lyo.oslc4j.provider.json4j;

import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.util.Map;

import javax.xml.datatype.DatatypeConfigurationException;

import org.eclipse.lyo.oslc4j.core.exception.OslcCoreApplicationException;
import org.eclipse.lyo.oslc4j.core.model.ResponseInfo;

import org.eclipse.lyo.oslc4j.provider.json4j.internal.JSONModelBuilder;
import org.eclipse.lyo.oslc4j.provider.json4j.internal.ResourceBuilder;

import jakarta.json.JsonObject;

public final class JsonHelper {

    /**
     * System property {@value} : When "true", write "INF", "-INF", and "NaN"
     * strings for Infinity, -Infinity, and NaN float and double values,
     * respectively. Enabled by default.
     *
     * @see #OSLC4J_READ_SPECIAL_NUMS
     */
    public static final String OSLC4J_WRITE_SPECIAL_NUMS = "org.eclipse.lyo.oslc4j.writeSpecialNumberValues";

    /**
     * System property {@value} : When "true", read "INF", "-INF", and "NaN"
     * strings for Infinity, -Infinity, and NaN float and double values,
     * respectively. Enabled by default.
     *
     * @see #OSLC4J_WRITE_SPECIAL_NUMS
     */
    public static final String OSLC4J_READ_SPECIAL_NUMS = "org.eclipse.lyo.oslc4j.readSpecialNumberValues";

    private JsonHelper() {
        super();
    }

    public static JsonObject createJSON(final String descriptionAbout, final String responseInfoAbout, final ResponseInfo<?> responseInfo,
            final Object[] objects, final Map<String, Object> properties) throws DatatypeConfigurationException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException, OslcCoreApplicationException {

        return JSONModelBuilder.build(descriptionAbout, responseInfoAbout, responseInfo, objects, properties);
    }

    public static Object[] fromJSON(final JsonObject jakartaObject, final Class<?> beanClass)
            throws DatatypeConfigurationException, IllegalAccessException, IllegalArgumentException, InstantiationException,
            InvocationTargetException, OslcCoreApplicationException, URISyntaxException {

        return ResourceBuilder.build(jakartaObject, beanClass);
    }

}