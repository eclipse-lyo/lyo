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
 *     Steve Pitschke - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.oslc4j.core.model;

import java.net.URI;
import java.util.Map;

/**
 * Concrete ResponseInfo collection wrapper where resource are sent
 * as an array
 */
public class ResponseInfoArray<T extends Object> extends ResponseInfo<T[]>
{
    public
    ResponseInfoArray(
        T[] array,
        Map<String, Object> properties,
        int totalCount,
        String nextPage
    )
    {
        super(array, properties, totalCount, nextPage);
    }
    
    public
    ResponseInfoArray(
        T[] array,
        Map<String, Object> properties,
        int totalCount,
        URI nextPage
    )
    {
        super(array, properties, totalCount, nextPage);
    }
    
    public T[]
    array() { return resource(); }
}
