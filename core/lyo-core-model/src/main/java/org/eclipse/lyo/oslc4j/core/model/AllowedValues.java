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
package org.eclipse.lyo.oslc4j.core.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.xml.namespace.QName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(
        title = "OSLC Allowed Values Resource Shape",
        describes = OslcConstants.TYPE_ALLOWED_VALUES)
public final class AllowedValues extends AbstractResource {
    private static final QName PROPERTY_ALLOWED_VALUE =
            new QName(OslcConstants.OSLC_CORE_NAMESPACE, "allowedValue");

    public AllowedValues() {
        super();
    }

    public Collection<?> getValues() {
        Object o = getExtendedProperties().get(PROPERTY_ALLOWED_VALUE);
        if (o == null) {
            return Collections.emptyList();
        } else if (o instanceof Collection) {
            return (Collection<?>) o;
        } else {
            return Collections.singleton(o);
        }
    }

    public void setValues(final Collection<? extends Object> values) {
        getExtendedProperties().put(PROPERTY_ALLOWED_VALUE, values);
    }

    /**
     * @deprecated Use {@link #getValues()}, which allows for values other than String
     */
    @Deprecated
    public String[] getAllowedValues() {
        // Be compatible with the old behavior and only include String values.
        ArrayList<String> stringValues = new ArrayList<>();
        Collection<?> values = (Collection<?>) getExtendedProperties().get(PROPERTY_ALLOWED_VALUE);
        for (Object o : values) {
            if (o instanceof String) {
                stringValues.add((String) o);
            }
        }

        return stringValues.toArray(new String[stringValues.size()]);
    }

    /**
     * @deprecated Use {@link #setValues(Collection)}, which allows for values other than String
     */
    @Deprecated
    public void setAllowedValues(final String[] allowedValues) {
        getExtendedProperties().put(PROPERTY_ALLOWED_VALUE, allowedValues);
    }
}
