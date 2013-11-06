/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
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
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;

public final class TypeFactory
{
    private TypeFactory()
    {
        super();
    }

    /**
     * Returns the qualified name if the unqualified name is not null, otherwise
     * returns null.
     * 
     * @param objectClass
     *            object class.
     * @return the qualified name.
     */
    public static String getQualifiedName(final Class<?> objectClass)
    {
        String name = getName(objectClass);
        return name != null ? getNamespace(objectClass) + name : null; 
    }

    public static String getNamespace(final Class<?> objectClass)
    {
        final OslcNamespace oslcNamespaceAnnotation = objectClass.getAnnotation(OslcNamespace.class);

        return oslcNamespaceAnnotation != null ? oslcNamespaceAnnotation.value() : OslcConstants.OSLC_DATA_NAMESPACE;
    }

    /**
     * If the annotation {@linkplain OslcName} is defined and it is different
     * from empty string, returns it. Otherwise returns the class simple name.
     * If the value of the annotation is an empty string, returns null.
     * 
     * @param objectClass
     *            object class.
     * @return the Oslc name.
     */
    public static String getName(final Class<?> objectClass)
    {
        final OslcName oslcNameAnnotation = objectClass.getAnnotation(OslcName.class);
        String name = null;
        if (oslcNameAnnotation != null)
        {
            String annotationValue = oslcNameAnnotation.value();
            if (!"".equals(annotationValue))
            {
                name = annotationValue;
            }
        } 
        else {
            name = objectClass.getSimpleName();
        }
        return name;
    }
}
