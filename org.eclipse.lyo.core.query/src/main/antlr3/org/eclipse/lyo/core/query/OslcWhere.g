grammar OslcWhere;

options {
	output=AST;
	ASTLabelType=CommonTree;
}

tokens {
	SIMPLE_TERM = 'simple_term';
	IN_TERM = 'in_term';
	COMPOUND_TERM = 'compound_term';
	TERMS = 'terms';
	TYPED_VALUE = 'typed_value';
	LANGED_VALUE = 'langed_value';
	IN_VALUES = 'in_values';
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
    public OslcWhereParser(String where)
    {
        this(new CommonTokenStream(new OslcWhereLexer(new ANTLRStringStream(where))));
    }       
}


oslc_where    : compound_term 
	;

compound_term : simple_term ( boolean_op simple_term)*  -> ^( 'terms' simple_term (simple_term)* )
	; 

simple_term   : term | scoped_term  
	;

space         : ' '! ;

boolean_op    : ' and '! ;

term	:	term_simple | in_term ;

term_simple
	:	identifier_wc comparison_op value -> ^( 'simple_term' identifier_wc comparison_op value ) 
	;

in_term	:	identifier_wc in_op space? in_val -> ^( 'in_term' identifier_wc in_val )
	;


scoped_term   : identifier_wc '{' compound_term '}' -> ^( 'compound_term' identifier_wc  compound_term )
	;

identifier_wc : identifier | wildcard ;

identifier    : prefixedName ;

wildcard      : ASTERISK ;

comparison_op : '=' | '!=' | '<' | '>' | '<=' | '>=' ;

in_op         : ' in' ;

in_val        : '[' value (',' value)* ']' -> ^( 'in_values' value (value)* )
	;

value         : iriRef | literal_value ;

literal_value : boolean_val | decimal | string_esc | typed_string  | langed_string
	;
	
typed_string
	:	string_esc ( '^^' prefixedName ) -> ^( 'typed_value' string_esc prefixedName?  ) 
	;

langed_string
	:	string_esc LANGTAG  -> ^( 'langed_value' string_esc LANGTAG ) 
	;


boolean_val   : BOOLEAN ;

decimal       : DECIMAL ;

string_esc    : STRING_LITERAL ;

properties 	:	 
	property (',' property )* 
	;

property 	:	
	prefixedName ( nested_property )*  
	;

nested_property :
	'{' property (',' property )*  '}'
	;
	
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


LANGTAG
    : '@' PN_CHARS_BASE+ (MINUS (PN_CHARS_BASE DIGIT)+)?
    ;

BOOLEAN       
    : 'true'
    | 'false'
    ;

DECIMAL
    : (MINUS | PLUS)? DIGIT+ DOT DIGIT*
    | (MINUS | PLUS)? DOT DIGIT+
    ;

STRING_LITERAL
    : '"'  ( options {greedy=false;} : ~('\u0022' | '\u005C' | '\u000A' | '\u000D') | ECHAR )* '"'
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

REFERENCE
    : '^^'
 ;

LESS_EQUAL
    : '<='
    ;

GREATER_EQUAL
    : '>='
    ;

NOT_EQUAL
    : '!='
    ;

AND
    : '&&'
    ;

OR
    : '||'
    ;

OPEN_BRACE
    : '('
    ;

CLOSE_BRACE
    : ')'
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

EQUAL
    : '='
    ;

LESS
    : '<'
    ;

GREATER
    : '>'
    ;

// $>
