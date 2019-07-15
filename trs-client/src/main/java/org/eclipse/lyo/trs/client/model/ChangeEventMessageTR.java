/*
 * Copyright (c) 2019 KTH Royal Institute of Technology and others
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 */

package org.eclipse.lyo.trs.client.model;

import org.apache.jena.rdf.model.Model;
import org.eclipse.lyo.core.trs.ChangeEvent;

public class ChangeEventMessageTR {
    private final ChangeEvent changeEvent;
    private final Model trackedResourceModel;

    public ChangeEventMessageTR(final ChangeEvent changeEvent, final Model trackedResourceModel) {
        this.changeEvent = changeEvent;
        this.trackedResourceModel = trackedResourceModel;
    }

    public ChangeEvent getChangeEvent() {
        return changeEvent;
    }

    public Model getTrackedResourceModel() {
        return trackedResourceModel;
    }
}
