/*******************************************************************************
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *     Samuel Padgett       - remove final from class
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Prefix Definition Resource Shape", describes = OslcConstants.TYPE_PREFIX_DEFINITION)
public class PrefixDefinition extends AbstractResource {
	private String prefix;
	private URI prefixBase;

	public PrefixDefinition() {
	    super();
	}

	public PrefixDefinition(final String prefix, final URI prefixBase) {
	    this();

		this.prefix = prefix;
		this.prefixBase = prefixBase;
	}

	@OslcDescription("Namespace prefix to be used for this namespace")
	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "prefix")
	@OslcReadOnly
    @OslcTitle("Prefix")
	public String getPrefix() {
		return prefix;
	}

	@OslcDescription("The base URI of the namespace")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "prefixBase")
	@OslcReadOnly
    @OslcTitle("Prefix Base")
	public URI getPrefixBase() {
	    return prefixBase;
	}

	public void setPrefix(final String prefix) {
		this.prefix = prefix;
	}

	public void setPrefixBase(final URI prefixBase) {
	    this.prefixBase = prefixBase;
	}
}
