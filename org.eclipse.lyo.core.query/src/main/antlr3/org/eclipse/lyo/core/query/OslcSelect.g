grammar OslcSelect;

options {
	output=AST;
	ASTLabelType=CommonTree;
}

tokens {
	PROPERTIES = 'properties';
	NESTED_PROPERTIES = 'nested_property';
	PREFIXED_NAME = 'prefixed_name';
	WILDCARD = 'wildcard';
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
}

oslc_select	: properties
	;

property	: identifier | nested_property 
	;
	
property_list	: property  ( ',' property )*  -> ^( 'properties' property (property)* )
	;
	
properties	: ( wildcard | property_list ) 
	; 
	
nested_property : identifier OPEN_CURLY_BRACE properties CLOSE_CURLY_BRACE -> ^( 'nested_property' identifier properties )
	;

identifier	: prefixedName -> ^( 'prefixed_name' prefixedName )
	;

wildcard	: ASTERISK -> ^( 'wildcard' ASTERISK )
	;

uri_ref_esc	: '<' iriRef '>';

string_esc	: STRING_LITERAL ;
	
iriRef
    : IRI_REF
    | prefixedName
    ;

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


IRI_REF
    : LESS ( options {greedy=false;} : ~(LESS | GREATER | '"' | OPEN_CURLY_BRACE | CLOSE_CURLY_BRACE | '|' | '^' | '\\' | '`' | ('\u0000'..'\u0020')) )* GREATER 
    ;



STRING_LITERAL
    : '"'  ( options {greedy=false;} : ~('\u0022' | '\u005C' | '\u000A' | '\u000D') | ECHAR )* '"'
    ;

STRING_LITERAL_LONG
    :   '"""' ( options {greedy=false;} : ( '"' | '""' )? ( ~('"'|'\\') | ECHAR ) )* '"""'
    ;

fragment
ECHAR
    : '\\' ('t' | 'b' | 'n' | 'r' | 'f' | '\\' | '"' | '\'')
    ;

fragment
PN_CHARS_U
    : PN_CHARS_BASE | '_'
    ;

fragment
VARNAME
    : ( PN_CHARS_U | DIGIT ) ( PN_CHARS_U | DIGIT | '\u00B7' | '\u0300'..'\u036F' | '\u203F'..'\u2040' )*
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
EOL
    : '\n' | '\r'
    ;

OPEN_CURLY_BRACE
    : '{'
    ;

CLOSE_CURLY_BRACE
    : '}'
    ;

OPEN_SQUARE_BRACE
    : '['
    ;

CLOSE_SQUARE_BRACE
    : ']'
    ;

SEMICOLON
    : ';'
    ;

DOT
    : '.'
    ;

PLUS
    : '+'
    ;

MINUS
    : '-'
    ;

ASTERISK
    : '*'
    ;

COMMA
    : ','
    ;

NOT
    : '!'
    ;

DIVIDE
    : '/'
    ;

EQUAL
    : '='
    ;

LESS
    : '<'
    ;

GREATER
    : '>'
    ;

ANY : .
    ;

// $>
