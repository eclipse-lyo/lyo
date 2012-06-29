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
import org.eclipse.lyo.core.query.OslcSelectParser;
import org.eclipse.lyo.core.query.PropertyList;
import org.eclipse.lyo.core.query.SelectClause;
import org.eclipse.lyo.core.query.Wildcard;
import org.eclipse.lyo.core.query.impl.PropertyListInvocationHandler;
import org.eclipse.lyo.core.query.impl.WildcardInvocationHandler;

/**
 * Basic tests of oslc.select clause parsing
 */
public class BasicSelectTest
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
                "qm:testcase",
                "*",
                "oslc:create,qm:verified",
                "qm:state{oslc:verified_by{oslc:owner,qm:duration}}",
                "qm:submitted{*}"
            };
        
        for (String expression : expressions) {
        
            OslcSelectParser parser = new OslcSelectParser(expression);
            
            try {
                OslcSelectParser.oslc_select_return resultTree =
                    parser.oslc_select();
                
                CommonTree rawTree = (CommonTree)resultTree.getTree();
                SelectClause selectClause;
                
                if (rawTree.getType() == OslcSelectParser.PROPERTIES) {
                    selectClause = (SelectClause)
                        Proxy.newProxyInstance(SelectClause.class.getClassLoader(), 
                                new Class<?>[] { SelectClause.class, PropertyList.class },
                                new PropertyListInvocationHandler(
                                        (CommonTree)resultTree.getTree(),
                                        prefixMap));
                } else {
                    selectClause = (SelectClause)
                        Proxy.newProxyInstance(SelectClause.class.getClassLoader(), 
                                new Class<?>[] { SelectClause.class, Wildcard.class },
                                new WildcardInvocationHandler());
                }
                
                System.out.println(selectClause.toString());
                
            } catch (RecognitionException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
}
