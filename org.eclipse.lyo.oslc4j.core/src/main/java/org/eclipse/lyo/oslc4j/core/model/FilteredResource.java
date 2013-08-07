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
 *     Steve Pitschke - initial API and implementation
 *******************************************************************************/

package org.eclipse.lyo.oslc4j.core.model;

import java.util.Map;

/**
 * Wrapper for resources to pass in properties map to filter output
 * to a subset of the resource's properties
 */
public class FilteredResource<T extends Object> extends AbstractResource
{
    public
    FilteredResource(
        T resource,
        Map<String, Object> properties
    )
    {
        this.resource = resource;
        this.properties = properties;
    }
    
    public T
    resource() { return resource; }
    
    public Map<String, Object>
    properties() { return properties; }
    
    private final T resource;
    private final Map<String, Object> properties;
}
