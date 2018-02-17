grammar Definiti;

PACKAGE      : 'package';
IMPORT       : 'import';
TYPE         : 'type';
IF           : 'if';
ELSE         : 'else';
VERIFICATION : 'verification';
VERIFY       : 'verify';
VERIFYING    : 'verifying';
DEF          : 'def';
CONTEXT      : 'context';
ENUM         : 'enum';
MESSAGE      : 'message';
OK           : 'ok';
KO           : 'ko';

BOOLEAN                      : 'true' | 'false';
NUMBER                       : [0-9]+('.'[0-9]+)?;
STRING                       : '"' ( '\\"' | . )*? '"';
IDENTIFIER                   : [a-zA-Z0-9]+;
CALCULATOR_OPERATOR_LEVEL_1  : ('*' | '/' | '%');
CALCULATOR_OPERATOR_LEVEL_2  : ('+' | '-');
LOGICAL_OPERATOR             : ('==' | '!=' | '<' | '<=' | '>' | '>=');
LOGICAL_COMBINATION_OPERATOR : ('&&' | '||');
NOT_OPERATOR                 : '!';

definiti:
  packageName?
  imports*
  toplevel*
  EOF;

packageName: PACKAGE dottedIdentifier;

imports: IMPORT dottedIdentifier;

dottedIdentifier: (IDENTIFIER '.')* IDENTIFIER;

toplevel
  : verification
  | definedType
  | aliasType
  | enumType
  | namedFunction
  | context
  ;

chainedExpression : expression+;

// Priority from top to bottom
// The naming of all elements is to find the expression type
expression
  : '(' parameterListDefinition ')' '=>' '{' lambdaExpression=expression '}'
  | '(' parenthesis=expression ')'
  | OK
  | KO ('(' koExpressionParameters=expressionList? ')')?
  | methodExpression=expression '.' methodName=IDENTIFIER ('[' genericTypeList ']')? '(' methodExpressionParameters=expressionList? ')'
  | attributeExpression=expression '.' attributeName=IDENTIFIER
  | functionName=IDENTIFIER ('[' functionGenerics=genericTypeList ']')? '(' functionExpressionParameters=expressionList? ')'
  | '!' notExpression=expression
  | leftExpression=expression operator=CALCULATOR_OPERATOR_LEVEL_1  rightExpression=expression
  | leftExpression=expression operator=CALCULATOR_OPERATOR_LEVEL_2  rightExpression=expression
  | leftExpression=expression operator=LOGICAL_OPERATOR             rightExpression=expression
  | leftExpression=expression operator=LOGICAL_COMBINATION_OPERATOR rightExpression=expression
  | atomicExpression
  | IF '(' conditionExpression=expression ')' '{' conditionIfBody=chainedExpression '}' (ELSE '{' conditionElseBody=chainedExpression '}')?
  ;

atomicExpression
  : booleanExpression=BOOLEAN
  | numberExpression=NUMBER
  | stringExpression=STRING
  | referenceExpression=IDENTIFIER
  ;

expressionList : expression (',' expression)*;

atomicExpressionList : atomicExpression (',' atomicExpression)*;

verification :
  DOC_COMMENT?
  VERIFICATION verificationName=IDENTIFIER ('(' parameterListDefinition ')')? '{'
    verificationMessage
    function
  '}';

verificationMessage
  : literal=STRING
  | MESSAGE '(' message=STRING (',' typeReference)* ')'
  ;

typeDeclaration: name=IDENTIFIER ('[' typeDeclarationList ']')? ( '(' atomicExpressionList ')' )?;
typeDeclarationList: ((typeDeclaration ',')* typeDeclaration);

typeReference: name=IDENTIFIER ('[' genericTypeList ']')?;

definedType :
  DOC_COMMENT?
  TYPE typeName=IDENTIFIER ('[' genericTypeList ']')? ( '(' parameterListDefinition ')' )? verifyingList '{'
    attributeDefinition+

    (typeVerification)*
  '}';

attributeDefinition:
  DOC_COMMENT?
  attributeName=IDENTIFIER ':' typeDeclaration verifyingList;

typeVerification
  : atomicTypeVerification
  | dependentTypeVerification
  ;

atomicTypeVerification:
  VERIFY '{'
    verificationMessage
    typeVerificationFunction
  '}';

dependentTypeVerification:
  VERIFY verificationName=IDENTIFIER '(' parameterListDefinition ')' '{'
    verificationMessage
    typeVerificationFunction
  '}';

typeVerificationFunction: '(' IDENTIFIER ')' '=>' '{' chainedExpression '}';

aliasType:
  DOC_COMMENT?
  TYPE typeName=IDENTIFIER ('[' genericTypes=genericTypeList ']')? ( '(' parameterListDefinition ')' )? '=' typeDeclaration verifyingList aliasTypeBody?;

aliasTypeBody: '{'
  typeVerification*
'}';

enumType:
  DOC_COMMENT?
  ENUM typeName=IDENTIFIER '{' enumCase* '}';

enumCase: DOC_COMMENT? IDENTIFIER;

function : ('[' genericTypeList ']')? '(' parameterListDefinition ')' '=>' '{' chainedExpression '}';

verifyingList : verifying*;
verifying : VERIFYING verificationName=IDENTIFIER ('(' atomicExpressionList ')')?;

parameterDefinition: parameterName=IDENTIFIER ':' typeReference;
parameterListDefinition: ((parameterDefinition ',')* parameterDefinition | );

namedFunction: DEF name=IDENTIFIER ('[' genericTypeList ']')? '(' parameterListDefinition ')' ':' genericType '=>' namedFunctionBody;
namedFunctionBody
  : '{' chainedExpression '}'
  | expression
  ;

genericType: IDENTIFIER ('[' genericTypeList ']')?;
genericTypeList: ((genericType ',')* genericType);

// Contexts

context:
  CONTEXT IDENTIFIER '{{{'
    contextContent
  '}}}'
;

contextContent: contextContentSymbol*;

contextContentSymbol
  : BOOLEAN
  | NUMBER
  | STRING
  | IDENTIFIER
  | CALCULATOR_OPERATOR_LEVEL_1
  | CALCULATOR_OPERATOR_LEVEL_2
  | LOGICAL_OPERATOR
  | LOGICAL_COMBINATION_OPERATOR
  | NOT_OPERATOR
  | PACKAGE
  | IMPORT
  | TYPE
  | IF
  | ELSE
  | VERIFICATION
  | VERIFY
  | VERIFYING
  | DEF
  | CONTEXT
  | '.'
  | ':'
  | '(' | ')'
  | '{' | '}'
  | '[' | ']'
  | '=>'
  | '='
  | ','
  | '?'
  | '/'
  | DOC_COMMENT
  | BLOCK_COMMENT
  | LINE_COMMENT
  | WS
  | ANY
  ;

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;
ANY : .;