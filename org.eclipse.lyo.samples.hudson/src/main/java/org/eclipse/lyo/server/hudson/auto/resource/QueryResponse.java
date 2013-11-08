/*******************************************************************************
 * Copyright (c) 2013 IBM Corporation.
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
*     Samuel Padgett - initial implementation
*******************************************************************************/
package org.eclipse.lyo.server.hudson.auto.resource;

import java.util.Collection;

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

/**
 * An OSLC query response for the Hudson automation provider. We need our own
 * class since we can't use OSLC4J's Wink provider to marshal the response.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 */
@OslcNamespace(HudsonAutoConstants.NAMESPACE)
@OslcResourceShape(title = "Hudson Query Response", describes = "QueryResponse")
public class QueryResponse extends AbstractResource
{
    private Collection<? extends Object> resources;

    public QueryResponse()
    {
    }

    @OslcName("member")
    @OslcPropertyDefinition(OslcConstants.RDFS_NAMESPACE + "member")
    public Collection<? extends Object> getResources()
    {
        return resources;
    }

    public void setResources(Collection<? extends Object> resources)
    {
        this.resources = resources;
    }
}
