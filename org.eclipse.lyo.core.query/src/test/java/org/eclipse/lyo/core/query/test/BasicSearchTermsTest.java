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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.eclipse.lyo.core.query.OslcSearchTermsParser;
import org.eclipse.lyo.core.query.SearchTermsClause;

/**
 * Basic tests of oslc.searchTerms clause parsing
 */
public class BasicSearchTermsTest
{
    static class StringList extends ArrayList<String>
        implements SearchTermsClause
    {
        public
        StringList(int size)
        {
            super(size);
        }
        
        public String
        toString()
        {
            StringBuffer buffer = new StringBuffer();
            boolean first = true;
            
            for (String string : this) {
                
                if (first) {
                    first = false;
                } else {
                    buffer.append(',');
                }
                
                buffer.append('"');
                buffer.append(string);
                buffer.append('"');
            }
            
            return buffer.toString();
        }
        
        private static final long serialVersionUID = 1943909246265711359L;
    }
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        Map<String, String> prefixMap = new HashMap<String, String>();
        
        prefixMap.put("qm", "http://qm.example.com/ns");
        prefixMap.put("olsc", "http://open-services.net/ns/core#");
       
        String[] expressions = {
                "\"foobar\"",
                "\"foobar\",\"whatsis\\\"yousa\"",
            };
        
        for (String expression : expressions) {
        
            OslcSearchTermsParser parser = new OslcSearchTermsParser(expression);
            
            try {
                OslcSearchTermsParser.oslc_search_terms_return resultTree =
                    parser.oslc_search_terms();
                
                CommonTree rawTree = (CommonTree)resultTree.getTree();
                @SuppressWarnings("unchecked")
                List<CommonTree> rawList = rawTree.getChildren();
                List<String> stringList = new StringList(rawList.size());
                
                for (CommonTree string : rawList) {
                    
                    String rawString = string.getText();
                    
                    stringList.add(rawString.substring(1, rawString.length()-1));
                }
                
                System.out.println(stringList);
                
            } catch (RecognitionException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
}
