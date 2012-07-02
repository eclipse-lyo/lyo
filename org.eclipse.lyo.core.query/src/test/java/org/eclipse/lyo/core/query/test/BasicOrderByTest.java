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
 *    Steve Pitschke - initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.core.query.test;

import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.Map;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.OrderByClause;
import org.eclipse.lyo.core.query.OslcOrderByParser;
import org.eclipse.lyo.core.query.SortTerms;
import org.eclipse.lyo.core.query.impl.SortTermsInvocationHandler;

/**
 * Basic tests of oslc.orderBy clause parsing
 */
public class BasicOrderByTest
{

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        Map<String, String> prefixMap = new HashMap<String, String>();
        
        prefixMap.put("qm", "http://qm.example.com/ns");
        prefixMap.put("olsc", "http://open-services.net/ns/core#");
       
        String[] expressions = {
                "+gm:priority",
                "+gm:priority,-oscl:name",
                "gm:tested_by{+oslc:description}",
            };
        
        for (String expression : expressions) {
        
            OslcOrderByParser parser = new OslcOrderByParser(expression);
            
            try {
                OslcOrderByParser.oslc_order_by_return resultTree =
                    parser.oslc_order_by();
                
                OrderByClause orderByClause = (OrderByClause)
                        Proxy.newProxyInstance(OrderByClause.class.getClassLoader(), 
                                new Class<?>[] { OrderByClause.class, SortTerms.class },
                                new SortTermsInvocationHandler(
                                        (CommonTree)resultTree.getTree(),
                                        prefixMap));
                
                System.out.println(orderByClause);
                
            } catch (RecognitionException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
}
