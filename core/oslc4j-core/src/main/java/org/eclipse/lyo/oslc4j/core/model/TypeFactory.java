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
	 *			  object class.
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
	 *			  object class.
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
