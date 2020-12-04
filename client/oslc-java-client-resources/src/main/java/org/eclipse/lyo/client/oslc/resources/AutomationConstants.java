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
package org.eclipse.lyo.client.oslc.resources;


@Deprecated
public interface AutomationConstants
{
    String AUTOMATION_DOMAIN          = "http://open-services.net/ns/auto#";
    String AUTOMATION_NAMESPACE       = "http://open-services.net/ns/auto#";
    String AUTOMATION_PREFIX          = "oslc_auto";

    String TYPE_AUTOMATION_PLAN       = AUTOMATION_NAMESPACE + "AutomationPlan";
    String TYPE_AUTOMATION_REQUEST    = AUTOMATION_NAMESPACE + "AutomationRequest";
    String TYPE_AUTOMATION_RESULT     = AUTOMATION_NAMESPACE + "AutomationResult";
    String TYPE_PARAMETER_INSTANCE    = AUTOMATION_NAMESPACE + "ParameterInstance";

    String STATE_NEW                  = AUTOMATION_NAMESPACE + "new";
    String STATE_QUEUED               = AUTOMATION_NAMESPACE + "queued";
    String STATE_IN_PROGRESS          = AUTOMATION_NAMESPACE + "inProgress";
    String STATE_CANCELING            = AUTOMATION_NAMESPACE + "canceling";
    String STATE_CANCELED             = AUTOMATION_NAMESPACE + "canceled";
    String STATE_COMPLETE             = AUTOMATION_NAMESPACE + "complete";

    String VERDICT_UNAVAILABLE        = AUTOMATION_NAMESPACE + "unavailable";
    String VERDICT_PASSED             = AUTOMATION_NAMESPACE + "passed";
    String VERDICT_WARNING            = AUTOMATION_NAMESPACE + "warning";
    String VERDICT_FAILED             = AUTOMATION_NAMESPACE + "failed";
    String VERDICT_ERROR              = AUTOMATION_NAMESPACE + "error";

}
