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

// spotless:off
// Start of user code imports
// End of user code
// spotless:on

public interface Oslc_configVocabularyConstants
{
    // Start of user code user constants
    // End of user code

    public static String OSLC_CONFIGURATION_MANAGEMENT_NAMSPACE = "http://open-services.net/ns/config#";
    public static String OSLC_CONFIGURATION_MANAGEMENT_NAMSPACE_PREFIX = "oslc_config";

    public static String CHANGESET = "ChangeSet";
    public static String TYPE_CHANGESET = OSLC_CONFIGURATION_MANAGEMENT_NAMSPACE + CHANGESET;
    public static String CONFIGURATION = "Configuration";
    public static String TYPE_CONFIGURATION = OSLC_CONFIGURATION_MANAGEMENT_NAMSPACE + CONFIGURATION;
}
