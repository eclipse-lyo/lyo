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

    public static String getQualifiedName(final Class<?> objectClass)
    {
        return getNamespace(objectClass) +
               getName(objectClass);
    }

    public static String getNamespace(final Class<?> objectClass)
    {
        final OslcNamespace oslcNamespaceAnnotation = objectClass.getAnnotation(OslcNamespace.class);

        return oslcNamespaceAnnotation != null ? oslcNamespaceAnnotation.value() : OslcConstants.OSLC_DATA_NAMESPACE;
    }

    public static String getName(final Class<?> objectClass)
    {
        final OslcName oslcNameAnnotation = objectClass.getAnnotation(OslcName.class);

        return oslcNameAnnotation != null ? oslcNameAnnotation.value() : objectClass.getSimpleName();
    }
}
