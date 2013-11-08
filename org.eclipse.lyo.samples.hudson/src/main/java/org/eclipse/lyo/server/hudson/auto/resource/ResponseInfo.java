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

import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;

/**
 * A ResponseInfo resource, part of an OSLC query response.
 * 
 * @author Samuel Padgett <spadgett@us.ibm.com>
 * @see QueryResponse
 */
@OslcNamespace(OslcConstants.OSLC_CORE_NAMESPACE)
@OslcResourceShape(title = "OSLC Response Info", describes = OslcConstants.TYPE_RESPONSE_INFO)
public class ResponseInfo extends AbstractResource
{
    private Integer totalCount;

    public ResponseInfo()
    {
    }

    @OslcName("totalCount")
    @OslcPropertyDefinition(OslcConstants.OSLC_CORE_NAMESPACE + "totalCount")
    public Integer getTotalCount()
    {
        return totalCount;
    }

    public void setTotalCount(Integer totalCount)
    {
        this.totalCount = totalCount;
    }
}
