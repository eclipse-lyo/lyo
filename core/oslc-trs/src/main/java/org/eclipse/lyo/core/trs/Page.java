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

import static org.eclipse.lyo.core.trs.TRSConstants.LDP_NAMESPACE;
import static org.eclipse.lyo.core.trs.TRSConstants.LDP_NEXT_PAGE;
import static org.eclipse.lyo.core.trs.TRSConstants.LDP_PAGE;
import static org.eclipse.lyo.core.trs.TRSConstants.LDP_PAGE_OF;
import static org.eclipse.lyo.core.trs.TRSConstants.LDP_TERM_NEXT_PAGE;
import static org.eclipse.lyo.core.trs.TRSConstants.LDP_TERM_PAGE_OF;

import java.net.URI;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

/**
 * In TRS, the Base Resource is paginated using the W3C Linked Data Platform
 * (LDP) Container pagination.	This class defines a LDP resource.
 */
@OslcNamespace(LDP_NAMESPACE)
@OslcResourceShape(title = "A Page in the Base Resource", describes = LDP_PAGE)
public class Page extends AbstractResource {
    private URI nextPage;
    private Base pageOf;

    /**
     * @return the nextPage
     */
    @OslcName(LDP_TERM_NEXT_PAGE)
    @OslcDescription(
            "The representation of a page resource will contain a subset of the Base's rdfs:member"
                + " predicates. In addition, it will contain another triple, whose subject is the"
                + " page resource itself (i.e., not the Base resource), with a reference to the"
                + " next page.")
    @OslcPropertyDefinition(LDP_NEXT_PAGE)
    @OslcTitle("Next Page")
    public URI getNextPage() {
        return nextPage;
    }

    /**
     * @param nextPage the nextPage to set
     */
    public void setNextPage(URI nextPage) {
        this.nextPage = nextPage;
    }

    /**
     * @return the pageOf
     */
    @OslcName(LDP_TERM_PAGE_OF)
    @OslcDescription("A reference back to the base resource this page belongs to.")
    @OslcPropertyDefinition(LDP_PAGE_OF)
    @OslcTitle("Page Of")
    public Base getPageOf() {
        return pageOf;
    }

    /**
     * @param pageOf the pageOf to set
     */
    public void setPageOf(Base pageOf) {
        this.pageOf = pageOf;
    }
}
