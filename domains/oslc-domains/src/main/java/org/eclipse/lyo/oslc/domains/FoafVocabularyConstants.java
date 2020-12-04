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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
// End of user code

package org.eclipse.lyo.oslc.domains;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

// Start of user code imports
// End of user code

public interface FoafVocabularyConstants
{
    // Start of user code user constants
    // End of user code

    public static String FOAF_NAMSPACE = "http://xmlns.com/foaf/0.1/";
    public static String FOAF_NAMSPACE_PREFIX = "foaf";

    public static String AGENT = "Agent";
    public static String TYPE_AGENT = FOAF_NAMSPACE + AGENT;
    public static String PERSON = "Person";
    public static String TYPE_PERSON = FOAF_NAMSPACE + PERSON;
}
