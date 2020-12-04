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

@Documented
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OslcDialog {
	/**
	 * Title string that could be used for display
	 */
	String title();

	/**
	 * Very short label for use in menu items
	 */
	String label() default "";

	/**
	 * The URI of the dialog
	 */
	String uri();

	/**
	 * Values MUST be expressed in relative length units.  Em and ex units are interpreted relative to the default system font (at 100% size).
	 */
	String hintWidth() default "";

	/**
	 * Values MUST be expressed in relative length units.  Em and ex units are interpreted relative to the default system font (at 100% size).
	 */
	String hintHeight() default "";

	/**
	 * Resource types
	 */
	String[] resourceTypes() default {};

	/**
	 * Usages
	 */
	String[] usages() default {};
}
