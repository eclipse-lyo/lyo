/*
 * Copyright (c) 2016-2018 KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials are made available under the terms of the Eclipse
 * Public License v1.0 and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html and the Eclipse Distribution
 * License is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.trs.client.util;

import java.util.Comparator;
import org.eclipse.lyo.core.trs.ChangeEvent;

/**
 * Comparator for ordering change events
 *
 * @author Omar
 *
 */
public class ChangeEventComparator implements Comparator<ChangeEvent> {
    @Override
    public int compare(ChangeEvent ce1, ChangeEvent ce2) {
        return ce1.getOrder() - ce2.getOrder();
    }
}
