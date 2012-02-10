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
 *     Russell Boykin       - initial API and implementation
 *     Alberto Giammaria    - initial API and implementation
 *     Chris Peters         - initial API and implementation
 *     Gianluca Bernardini  - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.stockquote.test;

import java.net.URISyntaxException;

import org.eclipse.lyo.oslc4j.core.model.OslcMediaType;

public class TestStockQuoteXml
       extends TestBase
{
    public TestStockQuoteXml()
    {
        super();
    }

    public void testResourceShape()
           throws URISyntaxException
    {
        testResourceShape(OslcMediaType.APPLICATION_XML);
    }

    public void testCreate()
    {
        testCreate(OslcMediaType.APPLICATION_XML);
    }

    public void testRetrieve()
    {
        testRetrieve(OslcMediaType.APPLICATION_XML);
    }

    public void testRetrieves()
    {
        testRetrieves(OslcMediaType.APPLICATION_XML);
    }

    public void testDelete()
    {
        testDelete(OslcMediaType.APPLICATION_XML);
    }
}