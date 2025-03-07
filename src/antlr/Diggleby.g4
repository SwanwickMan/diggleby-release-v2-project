grammar Diggleby;

@header {
package antlr;
}

type: 'Bool' | 'Int' | 'String' | 'Unit';

expr_list: expr (COMMA expr)+
         | expr
         ;

expr:
      LET IDENT EQ base_expr IN expr # Let
      | base_expr # BaseExpr
      ;

base_expr:
      base_expr op=(LT|GT|EQQ) base_expr # RelOp
    | base_expr op=(AND|OR|XOR) base_expr # BoolOp
    | base_expr op=(MUL|DIV|MOD) base_expr # MulDivOp
    | base_expr op=(PLUS|MINUS) base_expr # PlusMinusOp
    | NOT fact # Not
    | CONCAT LPAREN expr_list? RPAREN # Concat
    | PRINT LPAREN expr RPAREN # Print
    | GETINPUT LPAREN RPAREN # GetInput
    | IDENT LPAREN expr_list? RPAREN # FnCall
    | IF LPAREN expr RPAREN LBRACE expr RBRACE ELSE LBRACE expr RBRACE # IfThenElse
    | LPAREN expr COLON type RPAREN # Cast
    | fact # Fac
    ;

fact: 
      LPAREN RPAREN  # Unit
    | TRUE   # True
    | FALSE  # False
    | INT   # Int
    | STR   # Str
    | IDENT # Ident
    | LPAREN expr RPAREN # Parens
    ;

param_list:
      IDENT COLON type (COMMA IDENT COLON type)+
    | IDENT COLON type
    ;

def : DEF IDENT LPAREN param_list? RPAREN COLON type LBRACE expr RBRACE # FnDef
    ;

prog: def* expr # Program ;

WS : [ \t\r\n]+ -> skip ;
COMMA: ',';
DEF: 'def';
TRUE: 'true';
FALSE: 'false';
INT: [0-9]+;
LET: 'let';
IN: 'in';
PLUS: '+';
MINUS: '-';
MUL: '*';
DIV: '/';
MOD: '%';
GT: '>';
LT: '<';
EQQ: '==';
EQ: '=';
AND: '&&';
OR: '||';
XOR: '^';
NOT: '!';
CONCAT: 'concat';
IF: 'if';
THEN: 'then';
ELSE: 'else';
PRINT: 'print';
GETINPUT: 'getInput';
COLON: ':';
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
STR : '"' ( '\\' . | ~["\\] )* '"' ;
IDENT: [a-zA-Z][a-zA-Z0-9]*;
