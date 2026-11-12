/* SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause */
package org.eclipse.lyo.oslc4j.core.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Marks the URI component accessor containing a record's RDF subject identity. */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface OslcAbout {}
