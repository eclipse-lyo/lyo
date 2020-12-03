/******************************************************************************
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
 *    Steve Pitchke - initial API and implementation
 *******************************************************************************/
grammar OslcSearchTerms;

options {
	output=AST;
	ASTLabelType=CommonTree;
}

tokens {
	STRING_LIST = 'string_list';
}

@header {
package org.eclipse.lyo.core.query;
}

@lexer::header
{
package org.eclipse.lyo.core.query;
}

@members {
    private List<String> errors = new ArrayList<String>();
    public void displayRecognitionError(String[] tokenNames,
                                        RecognitionException e) {
        String hdr = getErrorHeader(e);
        String msg = getErrorMessage(e, tokenNames);
        errors.add(hdr + " " + msg);
    }
    public List<String> getErrors() {
        return errors;
    }
    public OslcSearchTermsParser(String searchTerms)
    {
        this(new CommonTokenStream(new OslcSearchTermsLexer(new ANTLRStringStream(searchTerms))));
    }       
}


oslc_search_terms    : string_esc ( ',' string_esc )* -> ^('string_list' string_esc (string_esc)* ) 
	;

string_esc    : STRING_LITERAL ;

// $>

// $<Lexer

WS
    : (' '| '\t'| EOL)+ { $channel=HIDDEN; }
    ;

fragment
EOL
    : '\n' | '\r'
    ;

STRING_LITERAL
    : '"'  ( options {greedy=false;} : ~('\u0022' | '\u005C' | '\u000A' | '\u000D') | ECHAR )* '"'
    ;

fragment
ECHAR
    : '\\' ('t' | 'b' | 'n' | 'r' | 'f' | '\\' | '"' | '\'')
    ;

COMMA
    : ','
    ;

// $>
