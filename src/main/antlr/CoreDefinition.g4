grammar CoreDefinition;

BOOLEAN                      : 'true' | 'false';
NUMBER                       : [0-9]+('.'[0-9]+)?;
STRING                       : '"' ( '\\"' | . )*? '"';
IDENTIFIER                   : [a-zA-Z0-9]+;

coreDefinition: toplevel*;

toplevel
  : coreType
  ;

coreType :
  DOC_COMMENT?
  'type' typeName=IDENTIFIER ('[' genericTypeList ']')? '{'
    member*
  '}';

member
  : attribute
  | method
  ;

attribute :
  DOC_COMMENT?
  attributeName=IDENTIFIER ':' attributeType=IDENTIFIER ('[' genericTypeList ']')?;

method :
  DOC_COMMENT?
  methodName=IDENTIFIER ('[' genericTypeList ']')? '(' parameterListDefinition ')' ':' methodType=typeReference;

parameterDefinition: parameterName=IDENTIFIER ':' abstractTypeReference;
parameterListDefinition: ((parameterDefinition ',')* parameterDefinition | );

typeReference: IDENTIFIER ('[' genericTypeList ']')?;
typeReferenceList: (typeReference ',')* typeReference;
lambdaReference
  : input=typeReference '=>' output=typeReference
  | '(' inputList=typeReferenceList ')' '=>' output=typeReference
  ;
abstractTypeReference
  : typeReference
  | lambdaReference
  ;

genericType: IDENTIFIER ('[' genericTypeList ']')?;
genericTypeList: ((genericType ',')* genericType);

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;