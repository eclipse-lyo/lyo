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

package org.eclipse.lyo.oslc.domains.jazz_am;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;


// spotless:off
// Start of user code imports
// End of user code
// spotless:on

public interface Jazz_amDomainConstants
{
    // Start of user code user constants
    // End of user code

    /**
     * @deprecated use {@link Jazz_amDomainConstants#JAZZ_ARCHITECTURE_MANAGEMENT_NAMSPACE} or {@link Jazz_amDomainConstants#JAZZ_ARCHITECTURE_MANAGEMENT_DOMAIN_NAME} instead
     */
    @Deprecated(since = "5.0.1")
    public static String JAZZ_ARCHITECTURE_MANAGEMENT_DOMAIN = "http://jazz.net/ns/dm/linktypes#";
    public static String JAZZ_ARCHITECTURE_MANAGEMENT_DOMAIN_NAME = "Jazz Architecture Management ";
    public static String JAZZ_ARCHITECTURE_MANAGEMENT_NAMSPACE = "http://jazz.net/ns/dm/linktypes#"; //Vocabulary namespace for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined 
    public static String JAZZ_ARCHITECTURE_MANAGEMENT_NAMSPACE_PREFIX = "jazz_am"; //Vocabulary prefix for the resources and resource properties, when no explicit vocabulary (describes, or propertyDefinition) is defined

}
