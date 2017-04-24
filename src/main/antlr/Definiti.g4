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
  ;

chainedExpression : expression+;

// Priority from top to bottom
// The naming of all elements is to find the expression type
expression
  : '(' parameterListDefinition ')' '=>' '{' lambdaExpression=expression '}'
  | '(' parenthesis=expression ')'
  | methodExpression=expression '.' methodName=IDENTIFIER ('[' genericTypeList ']')? '(' methodExpressionParameters=expressionList? ')'
  | attributeExpression=expression '.' attributeName=IDENTIFIER
  | '!' notExpression=expression
  | leftExpression=expression operator=CALCULATOR_OPERATOR_LEVEL_1  rightExpression=expression
  | leftExpression=expression operator=CALCULATOR_OPERATOR_LEVEL_2  rightExpression=expression
  | leftExpression=expression operator=LOGICAL_OPERATOR             rightExpression=expression
  | leftExpression=expression operator=LOGICAL_COMBINATION_OPERATOR rightExpression=expression
  | booleanExpression=BOOLEAN
  | numberExpression=NUMBER
  | stringExpression=STRING
  | variableExpression=IDENTIFIER
  | 'if' '(' conditionExpression=expression ')' '{' conditionIfBody=chainedExpression '}' ('else' '{' conditionElseBody=chainedExpression '}')?
  ;

expressionList : expression (',' expression)*;

verification :
  DOC_COMMENT?
  'verification' verificationName=IDENTIFIER '{'
    verificationMessage=STRING
    function
  '}';

definedType :
  DOC_COMMENT?
  'type' typeName=IDENTIFIER inheritance* ('[' genericTypeList ']')? '{'
    attributeDefinition+

    (typeVerification)*
  '}';

attributeDefinition:
  DOC_COMMENT?
  attributeName=IDENTIFIER ':' attributeType=IDENTIFIER ('[' genericTypeList ']')?;

typeVerification:
  'verify' '{'
    verificationMessage=STRING
    function
  '}';

aliasType :
  DOC_COMMENT?
  'type' typeName=IDENTIFIER ('[' genericTypes=genericTypeList ']')? '=' referenceTypeName=IDENTIFIER ('[' aliasGenericTypes=genericTypeList ']')? inheritance*;

function : ('[' genericTypeList ']')? '(' parameterListDefinition ')' '=>' '{' chainedExpression '}';

inheritance : ('verifying' verificationName=IDENTIFIER);

parameterDefinition: parameterName=IDENTIFIER ':' parameterType=IDENTIFIER ('[' genericTypeList ']')?;
parameterListDefinition: ((parameterDefinition ',')* parameterDefinition | );

genericType: IDENTIFIER ('[' genericTypeList ']')?;
genericTypeList: ((genericType ',')* genericType);

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;