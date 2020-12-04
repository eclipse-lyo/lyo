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
 * SPDX-License-Identifier: EPL-2.0 OR BSD-3-Simple
 */
grammar OslcOrderBy;

options {
	output=AST;
	ASTLabelType=CommonTree;
}

tokens {
	TERMS = 'terms';
	SCOPED_TERM = 'scoped_term';
	SIMPLE_TERM = 'simple_term';
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
    public OslcOrderByParser(String where)
    {
        this(new CommonTokenStream(new OslcOrderByLexer(new ANTLRStringStream(where))));
    }       
}


oslc_order_by : sort_terms 
	;

sort_terms  : sort_term ( ',' sort_term )*  -> ^( 'terms' sort_term (sort_term)* )
	; 

sort_term   : scoped_sort_term | DIRECTION identifier -> ^( 'simple_term' identifier DIRECTION )
	;

scoped_sort_term : identifier '{' sort_terms '}' -> ^( 'scoped_term' identifier sort_terms )
    ;

identifier    : prefixedName ;

prefixedName
    : PNAME_LN
    | PNAME_NS
    ;

// $>

// $<Lexer


WS
    : (' '| '\t'| EOL)+ { $channel=HIDDEN; }
    ;

PNAME_NS
    : p=PN_PREFIX? ':'
    ;

PNAME_LN
    : PNAME_NS PN_LOCAL
    ;


DIRECTION       
    : PLUS
    | MINUS
    ;

fragment
PN_CHARS_U
    : PN_CHARS_BASE | '_'
    ;

fragment
PN_CHARS
    : PN_CHARS_U
    | MINUS
    | DIGIT
    | '\u00B7' 
    | '\u0300'..'\u036F'
    | '\u203F'..'\u2040'
    ;

fragment
PN_PREFIX
    : PN_CHARS_BASE ((PN_CHARS|DOT)* PN_CHARS)?
    ;

fragment
PN_LOCAL
    : ( PN_CHARS_U | DIGIT ) ((PN_CHARS|DOT)* PN_CHARS)?
    ;

fragment
PN_CHARS_BASE
    : 'A'..'Z'
    | 'a'..'z'
    | '\u00C0'..'\u00D6'
    | '\u00D8'..'\u00F6'
    | '\u00F8'..'\u02FF'
    | '\u0370'..'\u037D'
    | '\u037F'..'\u1FFF'
    | '\u200C'..'\u200D'
    | '\u2070'..'\u218F'
    | '\u2C00'..'\u2FEF'
    | '\u3001'..'\uD7FF'
    | '\uF900'..'\uFDCF'
    | '\uFDF0'..'\uFFFD'
    ;

fragment
DIGIT
    : '0'..'9'
    ;

fragment
DOT
    : '.'
    ;

fragment
EOL
    : '\n' | '\r'
    ;

OPEN_CURLY_BRACE
    : '{'
    ;

CLOSE_CURLY_BRACE
    : '}'
    ;

fragment
PLUS
    : '+'
    ;

fragment
MINUS
    : '-'
    ;

ASTERISK
    : '*'
    ;

COMMA
    : ','
    ;

// $>
