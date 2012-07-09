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

import org.eclipse.lyo.core.query.ParseException;
import org.eclipse.lyo.core.query.QueryUtils;
import org.eclipse.lyo.core.query.SearchTermsClause;

/**
 * Basic tests of oslc.searchTerms clause parsing
 */
public class BasicSearchTermsTest
{
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        String[] expressions = {
                "\"foobar\"",
                "\"foobar\",\"whatsis\\\"yousa\"",
                ""
            };
        
        for (String expression : expressions) {
        
            try {
                
                SearchTermsClause stringList =
                    QueryUtils.parseSearchTerms(expression);
                
                System.out.println(stringList);
                
            } catch (ParseException e) {
                e.printStackTrace(System.out);
            }
        }
    }
}
