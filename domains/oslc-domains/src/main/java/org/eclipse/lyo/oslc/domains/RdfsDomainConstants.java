// Start of user code Copyright
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
// End of user code

package org.eclipse.lyo.oslc.domains;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc.domains.RdfsVocabularyConstants;

// Start of user code imports
// End of user code

public interface RdfsDomainConstants
{
    // Start of user code user constants
    // End of user code

    public static String RDFS_DOMAIN = "http://www.w3.org/2000/01/rdf-schema#";
    public static String RDFS_NAMSPACE = "http://www.w3.org/2000/01/rdf-schema#";
    public static String RDFS_NAMSPACE_PREFIX = "rdfs";

    public static String CLASS_PATH = "class";
    public static String CLASS_NAMESPACE = RdfsVocabularyConstants.RDFS_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String CLASS_LOCALNAME = "Class"; //localName of the rdfs:class the resource describes
    public static String CLASS_TYPE = CLASS_NAMESPACE + CLASS_LOCALNAME; //fullname of the rdfs:class the resource describes
}
