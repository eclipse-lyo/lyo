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

// Start of user code imports
// End of user code

public interface Oslc_cmVocabularyConstants
{
    // Start of user code user constants
    // End of user code

    public static String CHANGE_MANAGEMENT_VOCAB_NAMSPACE = "http://open-services.net/ns/cm#";
    public static String CHANGE_MANAGEMENT_VOCAB_NAMSPACE_PREFIX = "oslc_cm";

    public static String CHANGENOTICE = "ChangeNotice";
    public static String TYPE_CHANGENOTICE = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + CHANGENOTICE;
    public static String CHANGEREQUEST = "ChangeRequest";
    public static String TYPE_CHANGEREQUEST = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + CHANGEREQUEST;
    public static String DEFECT = "Defect";
    public static String TYPE_DEFECT = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + DEFECT;
    public static String ENHANCEMENT = "Enhancement";
    public static String TYPE_ENHANCEMENT = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + ENHANCEMENT;
    public static String PRIORITY = "Priority";
    public static String TYPE_PRIORITY = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + PRIORITY;
    public static String REVIEWTASK = "ReviewTask";
    public static String TYPE_REVIEWTASK = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + REVIEWTASK;
    public static String SEVERITY = "Severity";
    public static String TYPE_SEVERITY = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + SEVERITY;
    public static String STATE = "State";
    public static String TYPE_STATE = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + STATE;
    public static String TASK = "Task";
    public static String TYPE_TASK = CHANGE_MANAGEMENT_VOCAB_NAMSPACE + TASK;
}
