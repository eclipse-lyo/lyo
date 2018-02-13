/*-*****************************************************************************
 * Copyright (c) 2017 Yash Khatri.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *
 * Contributors:
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/

/**
 * @since 2.3.0
 */

@OslcSchema({@OslcNamespaceDefinition(prefix = OslcConstants.DCTERMS_NAMESPACE_PREFIX,
                                      namespaceURI = OslcConstants.DCTERMS_NAMESPACE),
                    @OslcNamespaceDefinition(
        prefix = OslcConstants.OSLC_CORE_NAMESPACE_PREFIX,
        namespaceURI = OslcConstants.OSLC_CORE_NAMESPACE), @OslcNamespaceDefinition(
        prefix = OslcConstants.OSLC_DATA_NAMESPACE_PREFIX,
        namespaceURI = OslcConstants.OSLC_DATA_NAMESPACE), @OslcNamespaceDefinition(
        prefix = OslcConstants.RDF_NAMESPACE_PREFIX,
        namespaceURI = OslcConstants.RDF_NAMESPACE), @OslcNamespaceDefinition(
        prefix = OslcConstants.RDFS_NAMESPACE_PREFIX,
        namespaceURI = OslcConstants.RDFS_NAMESPACE), @OslcNamespaceDefinition(
        prefix = ShaclConstants.SHACL_CORE_NAMESPACE_PREFIX,
        namespaceURI = ShaclConstants.SHACL_CORE_NAMESPACE)})
package org.eclipse.lyo.validation.shacl;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespaceDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcSchema;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
