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

/**
 * Basic tests of oslc.searchTerms clause parsing
 */
public class BasicPrefixesTest
{
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        String[] expressions = {
                "qm=<http://qm.example.com/ns>," +
                    "olsc=<http://open-services.net/ns/core#>," +
                    "xs=<http://www.w3.org/2001/XMLSchema>",
                "qm=<http://qm.example.com/ns>," +
                    "XXX>"
            };
        
        for (String expression : expressions) {
        
            try {
                
                Map<String, String> prefixMap =
                    QueryUtils.parsePrefixes(expression);
                
                System.out.println(prefixMap);
                
            } catch (ParseException e) {
                e.printStackTrace(System.out);
            }
        }
    }
}
