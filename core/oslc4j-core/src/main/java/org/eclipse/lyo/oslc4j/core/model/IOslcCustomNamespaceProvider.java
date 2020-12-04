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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
package org.eclipse.lyo.oslc4j.core.model;

import java.util.Map;

import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;

/**
 * This interface provides a way to add custom prefixes at runtime. As it is
 * used inside the {@link OslcSchema} annotation, <b>every time</b> that this annotation 
 * is read during the response writing it is also checked for a class definition in 
 * the {@link OslcSchema#customNamespaceProvider()} field and if there is a concrete 
 * implementation of this interface, a new instance of this implementation will be created 
 * and the {@link #getCustomNamespacePrefixes()} invoked.
 * 
 * All values obtained will be added to the namespace prefix list, thereafter used in 
 * the response writing. Note that if the {@link #getCustomNamespacePrefixes()} map, hereafter 
 * called customMap, contains keys that are equal to the ones defined in 
 * {@link OslcSchema#value()}, the customMap's key=value will take precedence over the others.	
 * 
 * @author Daniel Figueiredo Caetano
 * @see OslcSchema
 */
public interface IOslcCustomNamespaceProvider {
	
	/**
	 * Gets a map where the key = prefix and value = namespace.
	 * @return null if there are no custom prefixes to add.
	 */
	Map<String, String> getCustomNamespacePrefixes(); 

}