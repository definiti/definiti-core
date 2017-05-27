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

definiti:
  packageName?
  imports*
  toplevel*
  EOF;

packageName: 'package' dottedIdentifier;

imports: 'import' dottedIdentifier;

dottedIdentifier: (IDENTIFIER '.')* IDENTIFIER;

toplevel
  : verification
  | definedType
  | aliasType
  | namedFunction
  | http
  ;

chainedExpression : expression+;

// Priority from top to bottom
// The naming of all elements is to find the expression type
expression
  : '(' parameterListDefinition ')' '=>' '{' lambdaExpression=expression '}'
  | '(' parenthesis=expression ')'
  | methodExpression=expression '.' methodName=IDENTIFIER ('[' genericTypeList ']')? '(' methodExpressionParameters=expressionList? ')'
  | attributeExpression=expression '.' attributeName=IDENTIFIER
  | functionName=IDENTIFIER ('[' functionGenerics=genericTypeList ']')? '(' functionExpressionParameters=expressionList? ')'
  | '!' notExpression=expression
  | leftExpression=expression operator=CALCULATOR_OPERATOR_LEVEL_1  rightExpression=expression
  | leftExpression=expression operator=CALCULATOR_OPERATOR_LEVEL_2  rightExpression=expression
  | leftExpression=expression operator=LOGICAL_OPERATOR             rightExpression=expression
  | leftExpression=expression operator=LOGICAL_COMBINATION_OPERATOR rightExpression=expression
  | booleanExpression=BOOLEAN
  | numberExpression=NUMBER
  | stringExpression=STRING
  | referenceExpression=IDENTIFIER
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
  'type' typeName=IDENTIFIER ('[' genericTypeList ']')? verifyingList '{'
    attributeDefinition+

    (typeVerification)*
  '}';

attributeDefinition:
  DOC_COMMENT?
  attributeName=IDENTIFIER ':' attributeType=IDENTIFIER ('[' genericTypeList ']')? verifyingList;

typeVerification:
  'verify' '{'
    verificationMessage=STRING
    typeVerificationFunction
  '}';

typeVerificationFunction: '(' IDENTIFIER ')' '=>' '{' chainedExpression '}';

aliasType :
  DOC_COMMENT?
  'type' typeName=IDENTIFIER ('[' genericTypes=genericTypeList ']')? '=' referenceTypeName=IDENTIFIER ('[' aliasGenericTypes=genericTypeList ']')? verifyingList;

function : ('[' genericTypeList ']')? '(' parameterListDefinition ')' '=>' '{' chainedExpression '}';

verifyingList : verifying*;
verifying : 'verifying' verificationName=IDENTIFIER ('(' message=STRING ')')?;

parameterDefinition: parameterName=IDENTIFIER ':' parameterType=IDENTIFIER ('[' genericTypeList ']')?;
parameterListDefinition: ((parameterDefinition ',')* parameterDefinition | );

namedFunction: 'def' name=IDENTIFIER '=' function;

genericType: IDENTIFIER ('[' genericTypeList ']')?;
genericTypeList: ((genericType ',')* genericType);

typeReference: typeName=IDENTIFIER ('[' genericTypeList ']')?;

// HTTP api

http: 'http' '{' httpEntry* '}';

httpEntry
  : httpRequirement
  | httpRequest
  ;

httpRequirement:
  DOC_COMMENT?
  'requirement' name=IDENTIFIER '(' parameterListDefinition ')' ':' typeReference
;

httpRequest:
  DOC_COMMENT?
  'request' name=IDENTIFIER '{'
    httpRequestInput
    httpRequestRequiring?
    httpRequestReturning
  '}';

httpRequestInput:
  httpVerb
  httpRequestURI
  ('with' typeReference)?
;

httpRequestURI:
  (httpRequestURIPart '/')* httpRequestURIPart
  ('?' '(' parameterListDefinition ')')?;

httpRequestURIPart
  : STRING
  | '(' parameterDefinition ')'
  ;

httpRequestRequiring:
  'requiring' '{'
    httpRequestRequirement*
  '}'
;
httpRequestRequirement: httpRequestRequirementReference 'abort' httpResult;
httpRequestRequirementReference: name=IDENTIFIER '(' httpParameterList ')';

httpRequestReturning:
  'returning' '{'
    httpResult*
  '}'
;

httpParameterList: ((httpParameter ',')* httpParameter | );
httpParameter: name=IDENTIFIER;

httpResult: (httpStatus|httpStatusNumber) ('with' (raw=STRING|typeReference))?;

httpVerb
  : 'CONNECT'
  | 'DELETE'
  | 'GET'
  | 'HEAD'
  | 'PATCH'
  | 'POST'
  | 'PUT'
  | 'OPTIONS'
  | 'TRACE'
  ;
httpStatus
  : 'Continue' // 100
  | 'SwitchingProtocols' // 101
  | 'Processing' // 102

  | 'Ok' // 200
  | 'Created' // 201
  | 'Accepted' // 202
  | 'NonAuthoritativeInformation' // 203
  | 'NoContent' // 204
  | 'ResetContent' // 205
  | 'PartialContent' // 206
  | 'MultiStatus' // 207
  | 'AlreadyReported' // 208
  | 'IMUsed' // 209

  | 'MultipleChoices' // 300
  | 'MovedPermanently' // 301
  | 'Found' // 302
  | 'SeeOther' // 303
  | 'NotModified' // 304
  | 'UseProxy' // 305
  | 'SwitchProxy' // 306
  | 'TemporaryRedirect' // 307
  | 'PermanentRedirect' // 308

  | 'BadRequest' // 400
  | 'Unauthorized' // 401
  | 'PaymentRequired' // 402
  | 'Forbidden' // 403
  | 'NotFound' // 404
  | 'MethodNotAllowed' // 405
  | 'NotAcceptable' // 406
  | 'ProxyAuthenticationRequired' // 407
  | 'RequestTimeout' // 408
  | 'Conflict' // 409
  | 'Gone' // 410
  | 'LengthRequired' // 411
  | 'PreconditionFailed' // 412
  | 'PayloadTooLarge' // 413
  | 'URITooLong' // 414
  | 'UnsupportedMediaType' // 415
  | 'RangeNotSatisfiable' // 416
  | 'ExpectationFailed' // 417
  | 'ImATeapot' // 418
  | 'MisdirectedRequest' // 421
  | 'UnprocessableEntity' // 422
  | 'Locked' // 423
  | 'FailedDependency' // 424
  | 'UpgradeRequired' // 426
  | 'PreconditionRequired' // 428
  | 'TooManyRequests' // 429
  | 'RequestHeaderFieldsTooLarge' // 431
  | 'UnavailableForLegalReasons' // 451

  | 'InternalServerError' // 500
  | 'NotImplemented' // 501
  | 'BadGateway' // 502
  | 'ServiceUnavailable' // 503
  | 'GatewayTimeout' // 504
  | 'HTTPVersionNotSupported' // 505
  | 'VariantAlsoNegotiates' // 506
  | 'InsufficientStorage' // 507
  | 'LoopDetected' // 508
  | 'NotExtended' // 510
  | 'NetworkAuthenticationRequired' // 511
  ;
httpStatusNumber: NUMBER;

DOC_COMMENT   : '/**' .*? '*/';
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
LINE_COMMENT  : '//' ~[\r\n]* -> skip;

WS : [ \r\n\t]+ -> skip;