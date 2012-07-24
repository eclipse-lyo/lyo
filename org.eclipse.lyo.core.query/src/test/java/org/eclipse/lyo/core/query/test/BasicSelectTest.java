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

import java.util.Map;

import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SelectClause;

/**
 * Basic tests of oslc.select clause parsing
 */
public class BasicSelectTest
{
    public static void main(String[] args)
    {
        String prefixes = "qm=<http://qm.example.com/ns>," +
            "olsc=<http://open-services.net/ns/core#>";
       
        String[] expressions = {
                "*{*}",
                "qm:testcase",
                "*",
                "oslc:create,qm:verified",
                "qm:state{oslc:verified_by{oslc:owner,qm:duration}}",
                "qm:submitted{*}",
                "qm:testcase,*",
                "*,qm:state{*}",
                "XXX"
            };
        
        for (String expression : expressions) {
        
            try {
                
                Map<String, String> prefixMap =
                    QueryUtils.parsePrefixes(prefixes);
                SelectClause selectClause =
                    QueryUtils.parseSelect(expression, prefixMap);
                
                System.out.println(selectClause.toString());
                
            } catch (ParseException e) {
                e.printStackTrace(System.out);
            }
        }
    }
}
