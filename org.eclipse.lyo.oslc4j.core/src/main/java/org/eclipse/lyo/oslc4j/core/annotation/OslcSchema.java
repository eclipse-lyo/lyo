/*******************************************************************************
 * Copyright (c) 2012, 2014 IBM Corporation.
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
 *	   Russell Boykin			 - initial API and implementation
 *	   Alberto Giammaria		 - initial API and implementation
 *	   Chris Peters				 - initial API and implementation
 *	   Gianluca Bernardini		 - initial API and implementation
 *	   Daniel Figueiredo Caetano - custom namespace provider
 *	   Samuel Padgett			 - add @Documented
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.eclipse.lyo.oslc4j.core.model.IOslcCustomNamespaceProvider;

@Documented
@Target(ElementType.PACKAGE)
@Retention(RetentionPolicy.RUNTIME)
public @interface OslcSchema {

	/**
	 * The namespace mappings for the package.
	 */
	OslcNamespaceDefinition[] value();

	/**
	 * Any class that implements the {@link IOslcCustomNamespaceProvider}.
	 * This must be a concrete implementation and have a public no args constructor.
	 *
	 * @return {@link IOslcCustomNamespaceProvider} .class is the default value,
	 * because this field must not be required and since it is not a concrete
	 * implementation of the interface it will be ignored.
	 */
	Class<? extends IOslcCustomNamespaceProvider> customNamespaceProvider() default IOslcCustomNamespaceProvider.class;

}
