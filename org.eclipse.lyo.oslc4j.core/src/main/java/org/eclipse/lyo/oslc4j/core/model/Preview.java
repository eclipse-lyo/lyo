/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
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
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;

@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Preview Resource Shape", describes = OslcConstants.TYPE_PREVIEW)
public class Preview extends AbstractResource {
	private URI document;
    private String hintHeight;
	private String hintWidth;
	private String initialHeight;

	public Preview() {
	    super();
	}

	@OslcDescription("The URI of an HTML document to be used for the preview")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "document")
	@OslcReadOnly
    @OslcTitle("Document")
    public URI getDocument() {
	    return document;
	}

	@OslcDescription("Recommended height of the preview. Values MUST be expressed in relative length units as defined in the W3C Cascading Style Sheets Specification (CSS 2.1). Em and ex units are interpreted relative to the default system font (at 100% size).")
	@OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "hintHeight")
	@OslcReadOnly
    @OslcTitle("Hint Height")
	public String getHintHeight()
    {
        return hintHeight;
    }

	@OslcDescription("Recommended width of the preview. Values MUST be expressed in relative length units as defined in the W3C Cascading Style Sheets Specification (CSS 2.1). Em and ex units are interpreted relative to the default system font (at 100% size).")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "hintWidth")
    @OslcReadOnly
    @OslcTitle("Hint Width")
    public String getHintWidth()
    {
        return hintWidth;
    }

	@OslcDescription("Recommended initial height of the preview. The presence of this property indicates that the preview supports dynamically computing its size. Values MUST be expressed in relative length units as defined in the W3C Cascading Style Sheets Specification (CSS 2.1). Em and ex units are interpreted relative to the default system font (at 100% size).")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "initialHeight")
    @OslcReadOnly
    @OslcTitle("Initial Height")
    public String getInitialHeight()
    {
        return initialHeight;
    }

    public void setDocument(final URI document) {
	    this.document = document;
	}

    public void setHintHeight(final String hintHeight)
    {
        this.hintHeight = hintHeight;
    }

    public void setHintWidth(final String hintWidth)
    {
        this.hintWidth = hintWidth;
    }

    public void setInitialHeight(final String initialHeight)
    {
        this.initialHeight = initialHeight;
    }
}
