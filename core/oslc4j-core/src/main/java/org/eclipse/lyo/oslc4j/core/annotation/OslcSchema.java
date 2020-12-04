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
