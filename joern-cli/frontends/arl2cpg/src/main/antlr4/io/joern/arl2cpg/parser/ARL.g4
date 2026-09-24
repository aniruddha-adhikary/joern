grammar ARL;

// Every context records which of its rule's alternatives matched, so the coverage report can say
// `statement#4` rather than `statement`: a grammar rule is "handled" long before all of the shapes
// it admits are, and an untested alternative is exactly where a construct stops producing edges
// without anyone noticing.
// =====================================================================
// IBM ODM Advanced Rule Language (ARL) — strict combined grammar.
// NO permissiveness: any construct not modelled here is a hard error.
// Layer A = declarations/flow, Layer B = when-block patterns,
// Layer C = Java-forked expression/statement core.
// Oracles: ../build/oracle/LoanValidation/ruleset.arl (small)
//          ../build/oracle/dcs/dcs_rules.arl          (large)
// =====================================================================

compilationUnit
    : packageDecl? importDecl* signatureDecl? (rulesetDecl | ruleDecl*) ruleflowDecl? flowElement* EOF
    ;

// Rule Designer technical-rule files carry a package declaration and bare `rule X { ... }`
// declarations without an enclosing ruleset; compiled ARL keeps the rulesetDecl shape.
packageDecl : 'package' qualifiedName ';' ;

importDecl : 'import' qualifiedName ';' ;

// ---- signature (working-memory data model) -------------------------
signatureDecl
    : 'public' 'signature' Identifier 'extends' qualifiedName '{' signatureMember* '}'
    ;

signatureMember
    : 'public' direction? type Identifier '=' expression ';'
    ;

// `in out` is what the build command emits for an inout parameter (ODM 8.12 compiler output,
// e.g. `public in out Loan loan = null;`). Listed first so it is not read as `in` plus a stray
// token, which is a parse error rather than a silently wrong direction.
direction : 'in' 'out' | 'in' | 'out' ;

// ---- ruleset + rules -----------------------------------------------
rulesetDecl
    : 'ruleset' Identifier '(' Identifier ')' '{' overridingDecl? ruleDecl* '}'
    ;

// overriding { `a` > `b`; } — the rule-override relation Designer records on a rule ("this rule
// overrides that one"), compiled once per ruleset rather than as a rule property. Measured
// (ODM 9.6, permitty F04/OVR/P04).
overridingDecl : 'overriding' '{' overridingPair* '}' ;
overridingPair : ruleName '>' ruleName ';' ;

ruleDecl
    : 'rule' ruleName '{' ruleProperty* ruleWhen? ruleBody '}'
    ;

ruleName : BacktickId | Identifier ;

ruleProperty
    : 'property' Identifier '=' expression ';'
    | 'ilog' '.' 'rules' '.' Identifier '=' expression
    | 'status' '=' expression
    // any other property name the compiler writes unqualified: `effectiveDate`/`expirationDate`
    // for a rule with a validity period, and a custom property a project declares under its own
    // name (`promo_category = "Gold"`). Measured (ODM 9.6, permitty F06/F08); last, so the
    // properties above keep their own alternatives and stay distinguishable in the graph.
    | id '=' expression
    ;

ruleWhen : 'when' '{' whenStatement* '}' ;

ruleBody : ruleAction* ;

// A rule action is a then-block, a match-many, or an if/else over further
// rule actions (compiled guard around the decision-table outcome).
ruleAction
    : thenBlock
    | thenNamedBlock
    | thenRow
    | matchMany
    | 'if' '(' expression ')' '{' ruleAction* '}' ('else' '{' ruleAction* '}')?
    ;

// ---- match many (compiled decision table) --------------------------
matchMany : 'match' 'many' '{' matchCase* '}' ;

matchCase
    : 'case' '(' expression ')' ':' matchOutcome
    | 'default' ':' matchOutcome
    ;

// A case can resolve to a then-row, a nested match, or an if/else over
// further match-outcomes (compiled decision-table guard columns).
matchOutcome
    : thenRow
    | thenBlock
    | matchMany
    | 'if' '(' expression ')' '{' matchOutcome* '}' ('else' '{' matchOutcome* '}')?
    | 'when' '{' whenStatement* '}' matchOutcome    // when-guarded outcome
    ;

thenRow  : 'then' Integer block ;
thenBlock: 'then' block ;
// `then then { ... }` / `then else { ... }` — the compiler labels the arms of a rule's compiled
// if/else with the arm they came from. Measured (ODM 9.6, permitty I02).
thenNamedBlock : 'then' (id | 'else') block ;

// =====================================================================
// LAYER B — when-block pattern-matching sublanguage
// =====================================================================

whenStatement
    : evaluatePattern
    | wherePattern
    | aggregatePattern
    | existsPattern
    | notPattern
    | bindingPattern
    | classPattern
    ;

// where ( <bool> ) — the filter the compiler writes after an aggregate's `do` block for BAL's
// "where" on a count/collection (ODM 9.6 docs, "Count"; measured permitty C08/C09). Listed before
// classPattern because `where(...)` is also the shape of a pattern on a class called `where`, and
// the earlier alternative wins — the same way `evaluate (...)` does.
wherePattern : 'where' '(' expression ')' ';' ;

// evaluate ( [binding :] expr [; expr]* ) — a binding may be followed by
// ';'-separated expressions; the ';' behaves as sequencing/logical-and.
evaluatePattern : 'evaluate' '(' patternBinding? expression (';' expression)* ')' ';' ;

aggregatePattern
    : 'import'? aggregateLabel ':' 'aggregate' '{' collectPattern+ '}' 'do' '{' projection ';' '}'
    ;

aggregateLabel : BacktickId | freeText ;

// The `in <collection>` tail is absent when the aggregate collects over working memory itself
// rather than over one object's collection member — measured (ODM 9.6, permitty I02), where a
// BAL "there is at least one item such that ..." compiles to a collect with no source expression.
collectPattern
    : bindingName ':' qualifiedName '(' expression? ')' ('in' expression)? ';'
    ;

projection
    : ('count' | qualifiedName typeArguments?) '{' bindingName '}'
    ;

existsPattern : 'exists' '{' classPattern+ '}' ;

notPattern
    : 'not' aggregatePattern
    | 'not' '{' classPattern+ '}'
    ;

bindingPattern
    : bindingName ':' qualifiedName '(' expression? ')' ( ('from'|'in') expression )? ';'
    ;

classPattern
    : qualifiedName '(' expression? ')' ( ('from'|'in') expression )? ';'
    ;

// A binding name is whatever the author called the BAL variable, and the compiler writes it
// unquoted whenever it is a plain word — including a word the grammar reserves elsewhere. Measured
// (ODM 9.6, permitty C15): a definition named `count` compiles to `evaluate ( count : ... );`, and
// `report.count = count;` in the action. So the label position takes `id` (soft keywords included),
// not bare `Identifier`. Unambiguous: an expression cannot be followed by ':' here, so `id ':'` is
// only ever a label.
bindingName : BacktickId | id ;
patternBinding : (BacktickId | id) ':' ;

// =====================================================================
// LAYER A — ruleflow + tasks
// =====================================================================

ruleflowDecl
    : 'ruleflow' flowId '(' dollarRef ')' '{' 'maintask' flowId ';' '}'
    ;

flowElement
    : flowtaskDecl
    | ruletaskDecl
    | functiontaskDecl
    ;

flowtaskDecl
    : 'flowtask' flowId '(' dollarRef ')' '{' initialBlock? flowSeq? finalBlock? '}'
    ;

initialBlock : 'initial' block ;
finalBlock   : 'final' block ;

flowSeq : '{' flowStatement* '}' ;

flowStatement
    : callTask
    | flowIf
    | forkStmt
    | labeledBlock
    | gotoStmt
    | statement
    ;

// fork { ... } && { ... } && { ... } — parallel branches joined by '&&'.
forkStmt : 'fork' flowSeq ( '&&' flowSeq )+ ;

callTask : 'call' 'task' ':' taskRef ';' ;
// call-task targets are bare (not backticked): flow > task, each a free-text
// run of words/numbers/dots/hyphens (spaces allowed), '>' is the separator.
taskRef  : taskNamePart ( '>' taskNamePart )? ;
taskNamePart : taskWord+ ;
// A task name is free text, so any word of it may be spelled like a keyword — a flow called
// "default" is legal in Designer, and `>tag licence as valid/invalid` is a measured name whose
// reserved word ('as') is neither first nor last. So a name ends where its delimiter says it
// ends rather than where a list of permitted words runs out: enumerating the words is what broke,
// because no list can be completed against what an author may type (`import`, `as`, `extends`).
taskWord : ~(':' | ';' | '>' | '{' | '}' | BacktickId | StringLit) ;

flowIf
    : 'if' '(' expression ')' flowSeq ('else' flowSeq)?
    ;

labeledBlock : Identifier ':' flowSeq ;
gotoStmt     : 'goto' Identifier ';' ;

ruletaskDecl
    : 'ruletask' flowId '(' Identifier ')' '{'
        initialBlock?
        ruletaskProperty*
        'rules' ':' ruleSelector ';'
        selectBlock?
        finalBlock?
      '}'
    ;

// Emitted in any order by the compiler; `limit` appears for ExitCriteria=RuleInstance
// and for an explicit firing limit.
ruletaskProperty
    : 'ordering' ':' Identifier ';'
    | 'criteria' ':' Identifier ';'
    | 'limit' ':' expression ';'
    | 'firing' ':' Identifier ';'
    ;

// Dynamic rule filter: select ( Type `var` ) { ... return <bool>; }
selectBlock : 'select' '(' type (Identifier | BacktickId) ')' block ;

// Every character in a rules-selector is already a valid token
// (identifiers, '.', '*', ',', parens), so consume up to the ';'.
ruleSelector : ~';'* ;

functiontaskDecl
    : 'functiontask' flowId ( '(' dollarRef? ')' )? '{' initialBlock? statement* finalBlock? '}'
    ;

flowId  : BacktickId | Identifier ;
dollarRef : DollarId ;

// =====================================================================
// LAYER C — Java-forked expressions & statements
// =====================================================================

block : '{' statement* '}' ;

statement
    : block
    | 'if' '(' expression ')' statement ('else' statement)?
    | 'for' '(' type (Identifier | BacktickId) ':' expression ')' statement
    | 'for' '(' forInit? ';' expression? ';' expressionList? ')' statement
    | 'while' '(' expression ')' statement
    | 'return' expression? ';'
    // Designer keyword form of working-memory writes: `insert x;` / `retract x;` /
    // `update x;` / `modify x;`. Must precede localVarDecl since those words are also
    // valid `id`s and could otherwise start a declaration named e.g. `insert x`.
    | ('insert' | 'retract' | 'update' | 'modify') expression block? ';'
    | localVarDecl ';'
    | expression ';'
    // `insert (new Item("x", 1.0)) { }` — a statement whose object carries an initialiser block
    // of property assignments, which the compiler emits with no terminating semicolon.
    | expression block
    | ';'
    ;

forInit : localVarDecl | expressionList ;
localVarDecl : type Identifier ('=' expression)? ;

expressionList : expression (',' expression)* ;

expression        : assignment ;
assignment        : ternary ( ('=' | '+=' | '-=' | '*=' | '/=') assignment )? ;
ternary           : logicalOr ( '?' expression ':' expression )? ;
logicalOr         : logicalAnd ( '||' logicalAnd )* ;
logicalAnd        : equality ( '&&' equality )* ;
equality          : relational ( ('==' | '!=') relational )* ;
// 'as' is ARL's C#-style cast; 'instanceof'/'as' sit at the relational tier
// per the IRL operator-precedence table (both take a type on the right).
relational        : additive ( ('<' | '>' | '<=' | '>=' | 'instanceof' | 'as') additive )* ;
additive          : multiplicative ( ('+' | '-') multiplicative )* ;
multiplicative    : unary ( ('*' | '/' | '%') unary )* ;
unary             : ('!' | '-' | '+') unary
                  | castExpr
                  | postfix
                  ;
castExpr          : '(' type ')' unary ;
postfix           : primary ( '.' selector | arrayAccess )* ;
selector          : (id | BacktickId | 'this') arguments? ;
arrayAccess       : '[' expression ']' ;

primary
    : literal
    | intervalLiteral
    | 'new' creator
    | '(' expression ')'
    | DollarId
    | qualifiedName arguments?
    // no bare `BacktickId` alternative: `qualifiedName` already starts with one, so it wins every
    // time and a separate alternative here is unreachable — an alternative no input can select
    // reads as an untested construct forever (see arlgraph/constructs.py).
    | 'this'
    ;

arguments : '(' expressionList? ')' ;

creator
    : qualifiedName typeArguments? ( arguments | arrayCreatorRest )
    ;

arrayCreatorRest
    : ('[' ']')+ arrayInit
    | '[' expression ']' ('[' expression ']')*
    ;

arrayInit : '{' (expression (',' expression)*)? '}' ;

intervalLiteral
    : ('[' | ']') expression ',' expression (']' | '[') ('.' selector)*
    ;

literal
    : Integer | FloatLit | StringLit
    | 'true' | 'false' | 'null'
    ;

qualifiedName : (id | BacktickId) ('.' (id | BacktickId))* ;

// Soft keywords: reserved only in structural positions, usable as identifiers
// (member names, qualified-name segments) everywhere else. This is how ODM's
// contextual keywords collide with Java member names like `.rules`, `.status`.
id  : Identifier
    | 'in' | 'out' | 'from' | 'do' | 'count' | 'not' | 'rules' | 'status'
    | 'final' | 'initial' | 'property' | 'task' | 'call' | 'exists' | 'ilog'
    | 'aggregate' | 'when' | 'criteria' | 'ordering' | 'match' | 'many'
    | 'ruleflow' | 'ruletask' | 'flowtask' | 'functiontask' | 'maintask'
    | 'rule' | 'ruleset' | 'signature' | 'evaluate' | 'case' | 'then'
    | 'goto' | 'limit' | 'firing' | 'select' | 'where' | 'overriding'
    | 'insert' | 'retract' | 'update' | 'modify'
    ;

type
    : qualifiedName typeArguments? ('[' ']')*
    | primitiveType ('[' ']')*
    ;

primitiveType : 'boolean' | 'byte' | 'short' | 'int' | 'long' | 'char' | 'float' | 'double' ;

typeArguments : '<' typeArgument (',' typeArgument)* '>' ;
typeArgument  : type | '?' ;

// Free-text label (aggregate names): the words the author typed, which IBM's writer emits unquoted
// and which may be spelled like anything — `import licence:aggregate` is measured. Bounded by the
// ':' that introduces the aggregate, as `taskWord` is bounded by its own delimiters; parens and
// the block braces stay out so a label can never run into the pattern that follows it.
freeText : freeTextWord+ ;
// BacktickId is excluded so `not `dear items`:aggregate` stays a negated aggregate with a
// back-quoted label rather than one label spelled "not `dear items`".
freeTextWord : ~(':' | ';' | '{' | '}' | '(' | ')' | ',' | BacktickId | StringLit) ;

// ---------------------------------------------------------------------
// LEXER
// ---------------------------------------------------------------------
// Just $name — any dotted tail (.this.x.foo()) is handled by postfix/selector.
DollarId   : '$' [a-zA-Z_] [a-zA-Z_0-9]* ;
BacktickId : '`' ~'`'* '`' ;

// Java floating-point literal syntax, which is what the compiler emits: `1.0E-4` is measured
// (ODM 9.6, permitty D09); the exponent-only `1e5` and the `f`/`d` suffix are the rest of the
// Java form. Listed before Integer so `1e5` is one token, not `1` followed by a stray identifier.
FloatLit   : [0-9]+ '.' [0-9]* Exponent? FloatSuffix?
           | '.' [0-9]+ Exponent? FloatSuffix?
           | [0-9]+ Exponent FloatSuffix?
           | [0-9]+ FloatSuffix
           ;
fragment Exponent    : [eE] [+-]? [0-9]+ ;
fragment FloatSuffix : [fFdD] ;
// The `L` suffix is what the compiler writes for a long: `new ilog.rules.brl.UniversalTime(0L)`
// is measured (ODM 9.6, permitty VOC-UT).
Integer    : [0-9]+ [lL]? ;
StringLit  : '"' ( '\\' . | ~["\\] )* '"' ;

Identifier : [a-zA-Z_] [a-zA-Z_0-9]* ;

LINE_COMMENT  : '//' ~[\r\n]* -> skip ;
BLOCK_COMMENT : '/*' .*? '*/' -> skip ;
WS            : [ \t\r\n]+ -> skip ;
