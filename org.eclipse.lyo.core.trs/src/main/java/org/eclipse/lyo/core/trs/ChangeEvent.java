/*
 Copyright (c) 2013 IBM Corporation, 2017 KTH Royal Institute of Technology

 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 and Eclipse Distribution License v. 1.0 which accompanies this distribution.

 The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 and the Eclipse Distribution License is available at
 http://www.eclipse.org/org/documents/edl-v10.php.

 Contributors:

 Kevin Bauer           -  Initial implementation
 Andrew Berezovskyi    -  Dropped abstract modifier (487952)
 */
package org.eclipse.lyo.core.trs;

import java.net.URI;
import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcTitle;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;

import static org.eclipse.lyo.core.trs.TRSConstants.TRS_CHANGED;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_ORDER;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_CHANGED;
import static org.eclipse.lyo.core.trs.TRSConstants.TRS_TERM_ORDER;

/**
 * Each entry referenced by trs:change is a Local Resource representing a
 * Change Event consisting of the properties contained in this class.
 * <p>
 * Note: This class cannot be instantiated directly.  Instead create an instance
 * of one of the child classes depending on the type of event taking place.	 The
 * child classes are: Creation, Modification, and Deletion.
 */
public class ChangeEvent extends AbstractResource {
    private URI changed;
    private int order;

    public ChangeEvent() {}

    /**
     * @param about
     * @param changed
     * @param order
     */
    public ChangeEvent(URI about, URI changed, int order) {
        super(about);
        this.changed = changed;
        this.order = order;
    }

    @Override
    public String toString() {
        return "ChangeEvent{" + "kind=" + this.getClass()
                                              .getSimpleName() + ", changed=" + changed + ", " +
                "order=" + order + '}';
    }

    /**
     * @return the changed
     */
    @OslcName(TRS_TERM_CHANGED)
    @OslcDescription("The Resource that has changed.")
    @OslcPropertyDefinition(TRS_CHANGED)
    @OslcTitle("Changed")
    public URI getChanged() {
        return changed;
    }

    /**
     * @param changed the changed to set
     */
    public void setChanged(URI changed) {
        this.changed = changed;
    }

    /**
     * @return the order
     */
    @OslcName(TRS_TERM_ORDER)
    @OslcDescription("The sequence in time of the Change Event.")
    @OslcPropertyDefinition(TRS_ORDER)
    @OslcTitle("Order")
    public int getOrder() {
        return order;
    }

    /**
     * @param order the order to set
     */
    public void setOrder(int order) {
        this.order = order;
    }
}
