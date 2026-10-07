grammar Bom;

bomFile
    : bomItem* EOF
    ;

bomItem
    : includeDecl
    | packageDecl
    | propertyDecl
    | typeDecl
    ;

includeDecl
    : 'include' stringLiteral ';'?
    ;

packageDecl
    : 'package' qualifiedName ';'?
    ;

typeDecl
    : classDecl
    | interfaceDecl
    ;

classDecl
    : modifier* 'class' name typeParameters? classExtends? classImplements? metadata* classBody
    ;

interfaceDecl
    : modifier* 'interface' name typeParameters? interfaceExtends? metadata* classBody
    ;

classExtends
    : 'extends' typeRef (',' typeRef)*
    ;

classImplements
    : 'implements' typeRef (',' typeRef)*
    ;

interfaceExtends
    : 'extends' typeRef (',' typeRef)*
    ;

typeParameters
    : '<' typeParameter (',' typeParameter)* '>'
    ;

typeParameter
    : name ('extends' typeRef ('&' typeRef)*)?
    ;

typeArguments
    : '<' typeArgument (',' typeArgument)* '>'
    ;

typeArgument
    : '?' typeBound? typeRef?
    | typeRef
    ;

typeBound
    : 'extends'
    | 'super'
    ;

classBody
    : '{' classBodyItem* '}'
    ;

classBodyItem
    : typeDecl
    | domainDeclaration
    | constructorDecl
    | operatorDecl
    | methodDecl
    | fieldDecl
    | propertyDecl
    | annotationDecl
    ;

constructorDecl
    : modifier* name '(' formalParameters? ')' throwsClause? domainClause? metadata* ';'?
    ;

methodDecl
    : modifier* typeRef name '(' formalParameters? ')' throwsClause? domainClause? metadata* ';'?
    ;

operatorDecl
    : modifier* 'operator' typeRef '(' formalParameters? ')' throwsClause? domainClause? metadata* ';'?
    ;

fieldDecl
    : modifier* typeRef fieldDeclarator (',' fieldDeclarator)* domainClause? metadata* ';'?
    ;

fieldDeclarator
    : name arraySuffix* ('=' fieldInitializer)?
    ;

fieldInitializer
    : stringLiteral
    | signedNumber
    | 'true'
    | 'false'
    | 'null'
    | name
    ;

formalParameters
    : formalParameter (',' formalParameter)*
    ;

formalParameter
    : parameterModifier* typeRef '...'? name? arraySuffix* domainClause? metadata*
    ;

parameterModifier
    : 'final'
    ;

throwsClause
    : 'throws' typeRef (',' typeRef)*
    ;

typeRef
    : typeName typeArguments? arraySuffix*
    ;

typeName
    : typePart ('.' name)*
    ;

typePart
    : name
    | primitiveType
    | 'object'
    | 'string'
    | 'void'
    ;

primitiveType
    : 'boolean'
    | 'byte'
    | 'char'
    | 'short'
    | 'int'
    | 'long'
    | 'float'
    | 'double'
    ;

arraySuffix
    : '[' ']'
    ;

domainDeclaration
    : 'domain' domainBody ';'?
    ;

domainClause
    : 'domain' domainBody
    ;

domainBody
    : domainSet
    | domainRange
    | domainCardinality
    ;

domainSet
    : '{' domainValue (',' domainValue)* ','? '}'
    ;

domainRange
    : leftBracket domainValue ',' domainValue rightBracket
    ;

leftBracket
    : '['
    | '('
    ;

rightBracket
    : ']'
    | ')'
    | '['
    ;

domainCardinality
    : domainValue ',' domainValue ('class' typeRef)?
    ;

domainValue
    : typeCast? domainAtom
    ;

typeCast
    : '(' typeRef ')'
    ;

domainAtom
    : staticDomainValue
    | classDomainValue
    | stringLiteral
    | signedNumber
    | 'true'
    | 'false'
    | 'null'
    | '*'
    | qualifiedName
    ;

staticDomainValue
    : 'static' qualifiedName
    ;

classDomainValue
    : 'class' typeRef
    ;

signedNumber
    : '-'? (DecimalLiteral | IntegerLiteral)
    ;

metadata
    : annotationDecl
    | propertyDecl
    ;

annotationDecl
    : '#' qualifiedName ';'?
    ;

propertyDecl
    : 'property' propertyKey (stringLiteral | propertyBlock) ';'?
    ;

propertyKey
    : qualifiedName
    | stringLiteral
    ;

propertyBlock
    : '{' (propertyBlockEntry (',' propertyBlockEntry)* ','?)? '}'
    ;

propertyBlockEntry
    : propertyKey (stringLiteral | propertyBlock)
    ;

qualifiedName
    : name ('.' name)*
    ;

name
    : Identifier
    | EscapedIdentifier
    | 'object'
    | 'string'
    | 'default'
    ;

stringLiteral
    : StringLiteral
    ;

modifier
    : 'public'
    | 'protected'
    | 'private'
    | 'static'
    | 'final'
    | 'abstract'
    | 'readonly'
    | 'writeonly'
    | 'transient'
    | 'synchronized'
    | 'native'
    | 'volatile'
    | 'strictfp'
    | 'default'
    ;

EscapedIdentifier
    : '@' IdentifierStart IdentifierPart*
    ;

Identifier
    : IdentifierStart IdentifierPart*
    ;

fragment IdentifierStart
    : [a-zA-Z_$]
    ;

fragment IdentifierPart
    : [a-zA-Z0-9_$]
    ;

StringLiteral
    : '"' (EscapeSequence | ~["\\\r\n])* '"'
    ;

fragment EscapeSequence
    : '\\' .
    ;

DecimalLiteral
    : [0-9]+ '.' [0-9]+
    ;

IntegerLiteral
    : [0-9]+
    ;

LineComment
    : '//' ~[\r\n]* -> skip
    ;

BlockComment
    : '/*' .*? '*/' -> skip
    ;

Whitespace
    : [ \t\r\n]+ -> skip
    ;
