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
 * Wrapper for a collection or resources returned from an HTTP GET
 * to wrap that collection in a oslc:ResponseInfo element
 */
public abstract class ResponseInfo<T extends Object>
    extends FilteredResource<T>
{
    public
    ResponseInfo(
        T resource,
        Map<String, Object> properties,
        int totalCount,
        String nextPage
    )
    {
        super(resource, properties);
        
        this.totalCount = totalCount;
        this.nextPage = nextPage;
    }
    
    public
    ResponseInfo(
        T resource,
        Map<String, Object> properties,
        int totalCount,
        URI nextPage
    )
    {
        this(resource, properties, totalCount,
             nextPage == null ? null : nextPage.toString());
    }
    
    public int
    totalCount() { return totalCount; }
    
    public String
    nextPage() { return nextPage; }
    
    private final int totalCount;
    private final String nextPage;
}
