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
import org.eclipse.lyo.oslc.domains.DctermsVocabularyConstants;

// Start of user code imports
// End of user code

public interface DctermsDomainConstants
{
    // Start of user code user constants
    // End of user code

    /**
     * @deprecated use {@link DctermsDomainConstants#DUBLIN_CORE_NAMSPACE} or {@link DctermsDomainConstants#DUBLIN_CORE_DOMAIN_Name} instead
     */
    @Deprecated(since = "5.0.1")
    public static String DUBLIN_CORE_DOMAIN = "http://purl.org/dc/terms/";
    public static String DUBLIN_CORE_DOMAIN_Name = "Dublin Core";
    public static String DUBLIN_CORE_NAMSPACE = "http://purl.org/dc/terms/"; //Vocabulary namespace for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined 
    public static String DUBLIN_CORE_NAMSPACE_PREFIX = "dcterms"; //Vocabulary prefix for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined

}
