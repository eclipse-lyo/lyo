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
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Extended Error Resource Shape", describes = OslcConstants.TYPE_EXTENDED_ERROR)
public final class ExtendedError {
    private String        hintHeight;
    private String        hintWidth;
    private URI           moreInfo;
    private String        rel;

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
