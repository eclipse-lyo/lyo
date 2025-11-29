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
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

/**
 * OSLC Extended Error resource.
 * 
 * <p>This class extends {@link AbstractResource} to support extended properties
 * as required by the OSLC Core specification.
 * 
 * @see <a href="http://open-services.net/bin/view/Main/OslcCoreSpecification?sortcol=table;up=#Unknown_properties_and_content">OSLC Core 2.0: Unknown properties and content</a>
 * @see <a href="https://github.com/oslc-op/oslc-specs/issues/466">OSLC Spec Issue #466</a>
 */
@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Extended Error Resource Shape", describes = OslcConstants.TYPE_EXTENDED_ERROR)
public class ExtendedError extends AbstractResource {
	private String		  hintHeight;
	private String		  hintWidth;
	private URI			  moreInfo;
	private String		  rel;

	public ExtendedError() {
		super();
	}

	@OslcDescription("Values MUST be expressed in relative length units as defined in the W3C Cascading Style Sheets Specification (CSS 2.1) Em and ex units are interpreted relative to the default system font (at 100% size).")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "hintHeight")
	@OslcReadOnly
	@OslcTitle("Hint Height")
	public String getHintHeight() {
		return hintHeight;
	}

	@OslcDescription("Values MUST be expressed in relative length units as defined in the W3C Cascading Style Sheets Specification (CSS 2.1) Em and ex units are interpreted relative to the default system font (at 100% size).")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "hintWidth")
	@OslcReadOnly
	@OslcTitle("Hint Width")
	public String getHintWidth() {
		return hintWidth;
	}

	@OslcDescription("A resource giving more information on the error SHOULD be of an HTML content-type.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "moreInfo")
	@OslcReadOnly
	@OslcTitle("More Info")
	public URI getMoreInfo() {
		return moreInfo;
	}

	@OslcDescription("If present and set to 'alternate' then indicates that work-around is provided, behavior for other values is undefined.")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "rel")
	@OslcReadOnly
	@OslcTitle("Rel")
	public String getRel() {
		return rel;
	}

	public void setHintHeight(final String hintHeight) {
		this.hintHeight = hintHeight;
	}

	public void setHintWidth(final String hintWidth) {
		this.hintWidth = hintWidth;
	}

	public void setMoreInfo(final URI moreInfo) {
		this.moreInfo = moreInfo;
	}

	public void setRel(final String rel) {
		this.rel = rel;
	}
}
