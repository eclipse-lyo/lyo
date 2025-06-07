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
package org.eclipse.lyo.core.trs;

import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

/**
 * Set of constants useful for builing TRS implementations
 */
public interface TRSConstants extends OslcConstants {

    // TRS Constants
    public static final String TRS_NAMESPACE =
            "http://open-services.net/ns/core/trs#"; //$NON-NLS-1$
    public static final String TRS_NAMESPACE_PREFIX = "trs"; // $NON-NLS-1$

    // Types
    public static final String TRS_TERM_TYPE_BASE = "Base"; // $NON-NLS-1$
    public static final String TRS__PTERM_TYPE_BASE =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TYPE_BASE; // $NON-NLS-1$
    public static final String TRS_TYPE_BASE = TRS_NAMESPACE + TRS_TERM_TYPE_BASE;
    public static final String TRS_TERM_TYPE_CHANGE_LOG = "ChangeLog"; // $NON-NLS-1$
    public static final String TRS__PTERM_TYPE_CHANGE_LOG =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TYPE_CHANGE_LOG; // $NON-NLS-1$
    public static final String TRS_TYPE_CHANGE_LOG = TRS_NAMESPACE + TRS_TERM_TYPE_CHANGE_LOG;
    public static final String TRS_TERM_TYPE_CREATION = "Creation"; // $NON-NLS-1$
    public static final String TRS__PTERM_TYPE_CREATION =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TYPE_CREATION; // $NON-NLS-1$
    public static final String TRS_TYPE_CREATION = TRS_NAMESPACE + TRS_TERM_TYPE_CREATION;
    public static final String TRS_TERM_TYPE_DELETION = "Deletion"; // $NON-NLS-1$
    public static final String TRS__PTERM_TYPE_DELETION =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TYPE_DELETION; // $NON-NLS-1$
    public static final String TRS_TYPE_DELETION = TRS_NAMESPACE + TRS_TERM_TYPE_DELETION;
    public static final String TRS_TERM_TYPE_MODIFICATION = "Modification"; // $NON-NLS-1$
    public static final String TRS__PTERM_TYPE_MODIFICATION =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TYPE_MODIFICATION; // $NON-NLS-1$
    public static final String TRS_TYPE_MODIFICATION = TRS_NAMESPACE + TRS_TERM_TYPE_MODIFICATION;
    public static final String TRS_TERM_TYPE_TRACKED_RESOURCE_SET =
            "TrackedResourceSet"; //$NON-NLS-1$
    public static final String TRS__PTERM_TYPE_TRACKED_RESOURCE_SET =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TYPE_TRACKED_RESOURCE_SET; // $NON-NLS-1$
    public static final String TRS_TYPE_TRACKED_RESOURCE_SET =
            TRS_NAMESPACE + TRS_TERM_TYPE_TRACKED_RESOURCE_SET;
    public static final String TRS_TERM_TYPE_TRACKED_RESOURCE_SET_PROVIDER =
            "TrackedResourceSetProvider"; //$NON-NLS-1$
    public static final String TRS__PTERM_TYPE_TRACKED_RESOURCE_SET_PROVIDER =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TYPE_TRACKED_RESOURCE_SET_PROVIDER; // $NON-NLS-1$
    public static final String TRS_TYPE_TRACKED_RESOURCE_SET_PROVIDER =
            TRS_NAMESPACE + TRS_TERM_TYPE_TRACKED_RESOURCE_SET_PROVIDER;

    public static final String TRS_TERM_BASE = "base"; // $NON-NLS-1$
    public static final String TRS_PTERM_BASE =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_BASE; // $NON-NLS-1$
    public static final String TRS_BASE = TRS_NAMESPACE + TRS_TERM_BASE;
    public static final String TRS_TERM_CHANGE_LOG = "changeLog"; // $NON-NLS-1$
    public static final String TRS_PTERM_CHANGE_LOG =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_CHANGE_LOG; // $NON-NLS-1$
    public static final String TRS_TERM_CHANGED = "changed"; // $NON-NLS-1$
    public static final String TRS_PTERM_CHANGED =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_CHANGED; // $NON-NLS-1$
    public static final String TRS_CHANGED = TRS_NAMESPACE + TRS_TERM_CHANGED;
    public static final String TRS_CHANGE_LOG = TRS_NAMESPACE + TRS_TERM_CHANGE_LOG;
    public static final String TRS_TERM_CHANGE = "change"; // $NON-NLS-1$
    public static final String TRS_PTERM_CHANGE =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_CHANGE; // $NON-NLS-1$
    public static final String TRS_CHANGE = TRS_NAMESPACE + TRS_TERM_CHANGE;
    public static final String TRS_TERM_CUTOFFEVENT = "cutoffEvent"; // $NON-NLS-1$
    public static final String TRS_PTERM_CUTOFFEVENT =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_CUTOFFEVENT; // $NON-NLS-1$
    public static final String TRS_CUTOFFEVENT = TRS_NAMESPACE + TRS_TERM_CUTOFFEVENT;
    public static final String TRS_TERM_ORDER = "order"; // $NON-NLS-1$
    public static final String TRS_PTERM_ORDER =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_ORDER; // $NON-NLS-1$
    public static final String TRS_ORDER = TRS_NAMESPACE + TRS_TERM_ORDER;
    public static final String TRS_TERM_PREVIOUS = "previous"; // $NON-NLS-1$
    public static final String TRS_PTERM_PREVIOUS =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_PREVIOUS; // $NON-NLS-1$
    public static final String TRS_PREVIOUS = TRS_NAMESPACE + TRS_TERM_PREVIOUS;
    public static final String TRS_TERM_TRACKED_RESOURCE_SET = "trackedResourceSet"; // $NON-NLS-1$
    public static final String TRS_PTERM_TRACKED_RESOURCE_SET =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TRACKED_RESOURCE_SET; // $NON-NLS-1$
    public static final String TRS_TRACKED_RESOURCE_SET =
            TRS_NAMESPACE + TRS_TERM_TRACKED_RESOURCE_SET;
    public static final String TRS_TERM_TRACKED_RESOURCE_SET_PROVIDER =
            "trackedResourceSetProvider"; //$NON-NLS-1$
    public static final String TRS_PTERM_TRACKED_RESOURCE_SET_PROVIDER =
            TRS_NAMESPACE_PREFIX + ':' + TRS_TERM_TRACKED_RESOURCE_SET_PROVIDER; // $NON-NLS-1$
    public static final String TRS_TRACKED_RESOURCE_SET_PROVIDER =
            TRS_NAMESPACE + TRS_TERM_TRACKED_RESOURCE_SET_PROVIDER;

    // RDFS Constants
    public static final String RDFS_TERM_MEMBER = "member"; // $NON-NLS-1$
    public static final String RDFS_PTERM_MEMBER =
            RDFS_NAMESPACE_PREFIX + ':' + RDFS_TERM_MEMBER; // $NON-NLS-1$
    public static final String RDFS_MEMBER = RDFS_NAMESPACE + RDFS_TERM_MEMBER;

    // RDF Constants
    public static final String RDF_TERM_TYPE = "type"; // $NON-NLS-1$
    public static final String RDF_PTERM_TYPE =
            RDF_NAMESPACE_PREFIX + ':' + RDF_TERM_TYPE; // $NON-NLS-1$
    public static final String RDF_TYPE = RDF_NAMESPACE + RDF_TERM_TYPE; // $NON-NLS-1$

    public static final String RDF_TERM_NIL = "nil";
    public static final String RDF_PTERM_NIL =
            RDF_NAMESPACE_PREFIX + ':' + RDF_TERM_NIL; // $NON-NLS-1$
    public static final String RDF_NIL = RDF_NAMESPACE + RDF_TERM_NIL;

    public static final String FileSep = System.getProperty("file.separator");

    // LDP Constants
    public static final String LDP_NAMESPACE_PREFIX = "ldp";
    public static final String LDP_NAMESPACE = "http://www.w3.org/ns/ldp#";
    public static final String LDP_TERM_CONTAINER = "Container";
    public static final String LDP_CONTAINER = LDP_NAMESPACE + LDP_TERM_CONTAINER;
    public static final String LDP_TERM_PAGE = "Page";
    public static final String LDP_PAGE = LDP_NAMESPACE + LDP_TERM_PAGE;
    public static final String LDP_TERM_NEXT_PAGE = "nextPage"; // $NON-NLS-1$
    public static final String LDP_NEXT_PAGE = LDP_NAMESPACE + LDP_TERM_NEXT_PAGE;
    public static final String LDP_TERM_PAGE_OF = "pageOf";
    public static final String LDP_PAGE_OF = LDP_NAMESPACE + LDP_TERM_PAGE_OF;

    // XSD Constants
    public static final String XSD_NAMESPACE_PREFIX = "xsd";
    public static final String XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema#";
}
