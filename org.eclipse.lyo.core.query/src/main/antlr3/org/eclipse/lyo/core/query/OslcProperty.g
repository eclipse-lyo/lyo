grammar OslcProperty;


@header {
package org.eclipse.lyo.core.query;
}

@lexer::header
{
package org.eclipse.lyo.core.query;
}

/*
options {
	output=AST;
	ASTLabelType=CommonTree;
}

properties 	:	 
	( property (','! property )* ) 

	;
props	: properties EOF -> properties
	;


property 	:	
	prefixedName ( nested_property )*  
	;

nested_property :
	'{' properties '}'
	;
*/

prefixedName : PN_PREFIX ':' PN_LOCAL;


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

fragment PN_CHARS_U
    : PN_CHARS_BASE | '_'
    ;


fragment PN_CHARS
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
DIGIT
    : '0'..'9'
    ;

fragment   
DOT
    : '.'
    ;

fragment    
MINUS
    : '-'
    ;
