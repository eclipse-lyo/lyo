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
package org.eclipse.lyo.core.trs;

import static org.eclipse.lyo.core.trs.TRSConstants.TRS_NAMESPACE;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TYPE_CHANGE_LOG;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;

/**
 * A Change Log provides a list of changes organized in inverse chronological
 * order, most recent first. The following example illustrates the contents of a
 * Change Log:
 * <pre>
 * # Resource: http://cm1.example.com/trackedResourceSet
 * {@literal @prefix trs: <http://open-services.net/ns/core/trs#> .}
 * {@literal @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .}
 *
 * {@code
 * <http://cm1.example.com/trackedResourceSet>
 * a trs:TrackedResourceSet ;
 * trs:base <http://cm1.example.com/baseResources> ;
 * trs:changeLog
 * [ a	   trs:ChangeLog
 * ] .
 * }
 * </pre>
 *
 * An empty change log essentially contains nothing. This class exists to allow
 * JAX-RS implementations a way to generate the empty change log in the TRS
 * resource's output.
 */
@OslcNamespace(TRS_NAMESPACE)
@OslcResourceShape(title = "Change Log	Shape", describes = TRS_TYPE_CHANGE_LOG)
@OslcName("ChangeLog")
@Deprecated
public class EmptyChangeLog extends AbstractChangeLog {

}
