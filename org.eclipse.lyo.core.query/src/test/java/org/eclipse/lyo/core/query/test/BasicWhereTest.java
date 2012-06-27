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
import java.util.List;
import java.util.Map;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.ComparisonTerm;
import org.eclipse.lyo.core.query.CompoundTerm;
import org.eclipse.lyo.core.query.InTerm;
import org.eclipse.lyo.core.query.OslcWhereParser;
import org.eclipse.lyo.core.query.SimpleTerm;
import org.eclipse.lyo.core.query.Value;
import org.eclipse.lyo.core.query.WhereClause;
import org.eclipse.lyo.core.query.impl.CompoundTermInvocationHandler;

/**
 * @author pitschke
 *
 */
public class BasicWhereTest
{

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        Map<String, String> prefixMap = new HashMap<String, String>();
        
        prefixMap.put("qm", "http://qm.example.com/ns");
        prefixMap.put("olsc", "http://open-services.net/ns/core#");
        prefixMap.put("xs", "http://www.w3.org/2001/XMLSchema");
        
        String[] expressions = {
                "qm:testcase=<http://example.com/tests/31459>",
                "qm:duration>=10.4",
                "oslc:create!=\"Bob\" and qm:verified!=true",
                "qm:state in [\"Done\",\"Open\"]",
                "oslc:verified_by{oslc:owner=\"Steve\" and qm:duration=-47.0} and oslc:description=\"very hairy expression\"",
                "qm:submitted<\"2011-10-10T07:00:00Z\"^^\"xs:dateTime\"",
                "oslc:label>\"The End\"@en-US"
            };
        
        for (String expression : expressions) {
        
            OslcWhereParser parser = new OslcWhereParser(expression);
            
            try {
                OslcWhereParser.oslc_where_return resultTree =
                    parser.oslc_where();
                
                CompoundTerm whereClause = (CompoundTerm)
                    Proxy.newProxyInstance(CompoundTerm.class.getClassLoader(), 
                            new Class<?>[] { CompoundTerm.class, WhereClause.class },
                            new CompoundTermInvocationHandler(
                                    (CommonTree)resultTree.getTree(), true,
                                    prefixMap));
                
                System.out.println("Is where: " + (whereClause instanceof WhereClause));
                
                dumpCompound(whereClause);
                
                System.out.println();
                
            } catch (RecognitionException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
    
    private static void
    dumpCompound(CompoundTerm compoundTerm)
    {
        List<SimpleTerm> children = compoundTerm.children();
        boolean first = true;
        
        for (SimpleTerm term : children) {
            
            if (first) {
                first = false;
            } else {
                System.out.print(" and ");
            }
            
            System.out.print(term.property().toString());
            
            switch (term.type()) {
            case COMPARISON:
                {
                    ComparisonTerm compTerm = (ComparisonTerm)term;
                    
                    System.out.print(compTerm.operator().toString());
                    System.out.print(compTerm.operand().toString());
                }
                break;
            case IN_TERM:
                {
                    System.out.print(" in [");
                    
                    InTerm inTerm = (InTerm)term;
                    boolean firstValue = true;
                    
                    for (Value value : inTerm.values()) {
                        
                        if (firstValue) {
                            firstValue = false;
                        } else {
                            System.out.print(',');
                        }
                        
                        System.out.print(value.toString());
                    }
                    
                    System.out.print(']');
                }
                break;
            case NESTED:
                {
                    System.out.print('{');
                    
                    dumpCompound((CompoundTerm)term);
                    
                    System.out.print('}');
                }
                break;
            default:
                throw new IllegalStateException("unknown simple term type");
            }
        }
    }
}
