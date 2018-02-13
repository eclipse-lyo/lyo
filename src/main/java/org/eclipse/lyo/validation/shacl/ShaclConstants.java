/*-*****************************************************************************
 * Copyright (c) 2017 Yash Khatri and others.
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
 *    Yash Khatri         -  initial implementation
 *    Andrew Berezovskyi  -  refactoring
 *******************************************************************************/

package org.eclipse.lyo.validation.shacl;

/**
 * Created on 2018-02-12
 *
 * @author Andrew Berezovskyi (andriib@kth.se)
 * @version $version-stub$
 * @since 0.0.1
 */
public class ShaclConstants {
    public static final String SHACL_CORE_NAMESPACE = "http://www.w3.org/ns/shacl#";
    public static final String SHACL_CORE_NAMESPACE_PREFIX = "sh";

    public static final String TYPE_SHACL_PROPERTY = SHACL_CORE_NAMESPACE + "Property";
    public static final String TYPE_SHACL_SHAPE = SHACL_CORE_NAMESPACE + "Shape";

}
