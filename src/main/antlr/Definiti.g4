grammar Definiti;

BOOLEAN                      : 'true' | 'false';
NUMBER                       : [0-9]+('.'[0-9]+)?;
STRING                       : '"' ( '\\"' | . )*? '"';
IDENTIFIER                   : [a-zA-Z0-9]+;
CALCULATOR_OPERATOR_LEVEL_1  : ('*' | '/' | '%');
CALCULATOR_OPERATOR_LEVEL_2  : ('+' | '-');
LOGICAL_OPERATOR             : ('==' | '!=' | '<' | '<=' | '>' | '>=');
LOGICAL_COMBINATION_OPERATOR : ('&&' | '||');
NOT_OPERATOR                 : '!';

definiti: toplevel*;

toplevel
  : verification
  | definedType
  | aliasType
  | DOC_COMMENT
  ;

chainedExpression : expression+;

// Priority from top to bottom
expression
  : '(' expression ')'
  | expression '.' IDENTIFIER '(' expressionList? ')'
  | expression '.' IDENTIFIER
  | '!' expression
  | expression CALCULATOR_OPERATOR_LEVEL_1  expression
  | expression CALCULATOR_OPERATOR_LEVEL_2  expression
  | expression LOGICAL_OPERATOR             expression
  | expression LOGICAL_COMBINATION_OPERATOR expression
  | BOOLEAN
  | NUMBER
  | STRING
  | IDENTIFIER
  | 'if' '(' expression ')' '{' chainedExpression '}' ('else' '{' chainedExpression '}')?
  ;

expressionList : expression (',' expression)*;

verification : 'verification' IDENTIFIER '{'
  STRING
  function
'}';

definedType : 'type' IDENTIFIER ('verifying' IDENTIFIER)* '{'
  attributeDefinition+

  ('verify' '{'
    STRING
    function
  '}')*
'}';

aliasType : 'type' IDENTIFIER '=' IDENTIFIER ('verifying' IDENTIFIER)*;

function : '(' parameterListDefinition ')' '=>' '{' expression '}';

parameterDefinition: IDENTIFIER ':' IDENTIFIER;
parameterListDefinition: ((parameterDefinition ',')* parameterDefinition | );

attributeDefinition: IDENTIFIER ':' IDENTIFIER;

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;