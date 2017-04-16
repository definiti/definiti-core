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
  'type' typeName=IDENTIFIER '{'
    member*
  '}';

member
  : attribute
  | method
  ;

attribute :
  DOC_COMMENT?
  attributeName=IDENTIFIER ':' attributeType=IDENTIFIER;

method :
  DOC_COMMENT?
  methodName=IDENTIFIER '(' parameterListDefinition ')' ':' methodType=IDENTIFIER;

parameterDefinition: parameterName=IDENTIFIER ':' parameterType=IDENTIFIER;
parameterListDefinition: ((parameterDefinition ',')* parameterDefinition | );

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;