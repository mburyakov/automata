Nonterminals
Root ClassList Class BrClassBody ClassBody ClassMember ClassMemberName ExtendsList EL E BrTypedIdList TypedIdList BrIdList BrVarList VarList IdList Id Var Type State Tokens DotTokens Token.

Terminals
'class' 'extends'
char integer float atom string var
'(' ')' ',' '->' ':-' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.'
'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
'andalso' 'orelse' 'query'
'bnot' 'not'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'++' '--'
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<='
'<<' '>>'
'!' '=' '::' '..' '...'
'spec' 'callback' % helper
dot.


Rootsymbol Root.

Root -> ClassList EL                    : {'$1','$2'}.

ClassList -> Class ClassList : ['$1' | '$2'].
ClassList -> 'begin' : [].
Class -> class Type BrTypedIdList BrClassBody : {'class','$2','$3',{extends,[]},'$4'}.
Class -> class Type BrTypedIdList extends ExtendsList BrClassBody : {'class','$2','$3',{extends,'$5'},'$6'}.
ExtendsList -> Type BrVarList : [{'$1','$2'}].
ExtendsList -> Type BrVarList ',' ExtendsList : [{'$1','$2'}|'$4'].
BrTypedIdList -> '(' ')' : [].
BrTypedIdList -> '(' TypedIdList ')' : '$2'.
TypedIdList -> Var ':' Type : [{'$1','$3'}].
TypedIdList -> Var ':' Type ',' TypedIdList : [{'$1','$3'} | '$5'].
BrClassBody -> '{' '}'.
BrClassBody -> '{' ClassBody '}' : '$2'.
ClassBody -> ClassMember ClassBody : ['$1' | '$2'].
ClassBody -> ClassMember : ['$1'].
ClassMember -> ClassMemberName BrVarList '->' DotTokens: {'member','$1','$2',parse_ans(erl_parse:parse_exprs('$4'))}.
ClassMember -> ClassMemberName '->' DotTokens: {'member','$1',[],parse_ans(erl_parse:parse_exprs('$3'))}.
ClassMemberName -> atom : value_of('$1').
DotTokens -> Tokens dot : '$1' ++ ['$2'].
Tokens -> Token : ['$1'].
Tokens -> Token Tokens : ['$1' | '$2'].
Token -> var : '$1'.
Token -> '+' : '$1'.
Token -> '-' : '$1'.
Token -> '*' : '$1'.
Token -> '/' : '$1'.
Token -> 'if' : '$1'.
Token -> 'end' : '$1'.
Token -> 'atom' : '$1'.
Token -> '->' : '$1'.
Token -> '(' : '$1'.
Token -> ')' : '$1'.
Token -> ',' : '$1'.
Token -> ';' : '$1'.
Token -> ':' : '$1'.
Token -> '>' : '$1'.
Token -> '<' : '$1'.
Token -> '=' : '$1'.
Token -> '==' : '$1'.
Token -> integer : '$1'.

EL -> E                                         : ['$1'].
EL -> E EL                                      : ['$1' | '$2'].
E  -> Id ':' Type BrIdList '=' State ';'         : { '$1', {'$3','$4'}, '$6'}.
E  -> Id ':' Type    '=' State ';'                : { '$1', {'$3', [] }, '$5'}.
BrIdList -> '(' ')'                            : [].
BrIdList -> '(' IdList ')'                    : '$2'.
BrVarList -> '(' ')'                            : [].
BrVarList -> '(' VarList ')'                    : '$2'.
VarList -> Var                                   : [ '$1' ].
VarList -> Var ',' VarList                       : [ '$1' | '$3' ].
IdList -> Id                                   : [ '$1' ].
IdList -> Id ',' IdList                       : [ '$1' | '$3' ].
Var -> var : value_of('$1').
Id -> atom : value_of('$1').
Type -> atom : value_of('$1').  	
State -> integer : value_of('$1').

Erlang code.

value_of(Token) ->
    element(3,Token).
parse_ans(Parsed) ->
    element(2,Parsed).