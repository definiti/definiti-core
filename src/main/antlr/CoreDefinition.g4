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
  methodName=IDENTIFIER ('[' genericTypeList ']')? '(' parameterListDefinition ')' ':' methodType=IDENTIFIER;

parameterDefinition: parameterName=IDENTIFIER ':' parameterType=IDENTIFIER ('[' genericTypeList ']')?;
parameterListDefinition: ((parameterDefinition ',')* parameterDefinition | );

genericType: IDENTIFIER ('[' genericTypeList ']')?;
genericTypeList: ((genericType ',')* genericType);

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;