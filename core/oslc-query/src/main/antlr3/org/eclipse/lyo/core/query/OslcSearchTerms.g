/*
 * Copyright (c) 2020 Contributors to the Eclipse Foundation
 *
 * See the NOTICE file(s) distributed with this work for additional
 * information regarding copyright ownership.
 *
 * This program and the accompanying materials are made available under the
 * terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0, or the Eclipse Distribution License 1.0
 * which is available at http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Clause
 */
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
