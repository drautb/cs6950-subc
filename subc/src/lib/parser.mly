%{
open Core
%}

%token EOF
%token <int> INT
%token <char> CHAR
%token VOID_LIT CHAR_LIT INT_LIT IF ELSE WHILE RETURN
%token LEFT_BRACE RIGHT_BRACE LEFT_BRACKET RIGHT_BRACKET LEFT_PAREN RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token STAR PLUS MINUS SLASH
%token AMP
%token ASSIGN
%token L_AND L_OR L_NOT
%token B_EQ B_NEQ B_LEQ B_LESS B_GEQ B_GREATER
%token <string> ID

%start <Ast.subc_unit Core.List.t> program
%%

program:
  | l = declaration_or_function*; EOF
    { l }
  ;

declaration_or_function:
  | d = declaration; SEMICOLON
    { Ast.Declaration d }
  | f = function_definition
    { Ast.FunctionDefinition f }
  ;

declaration:
  | t = type_specifier; id = ID
    { Ast.Variable (t, id) }
  | t = type_specifier; id = ID; LEFT_BRACKET; size = INT; RIGHT_BRACKET
    { Ast.Array (t, id, size) }
  | t = type_specifier; id = ID; LEFT_PAREN; args = parameter_types; RIGHT_PAREN
    { Ast.FunctionDeclaration (t, id, args) }
  | VOID_LIT; id = ID; LEFT_PAREN; args = parameter_types; RIGHT_PAREN
    { Ast.FunctionDeclaration (Ast.Void, id, args) }
  ;

type_specifier:
  | CHAR_LIT; ptrs = STAR*
    { List.fold_left ptrs ~init:Ast.Char ~f:(fun t _ -> (Ast.Pointer t)) }
  | INT_LIT; ptrs = STAR*
    { List.fold_left ptrs ~init:Ast.Int ~f:(fun t _ -> (Ast.Pointer t)) }
  ;

variable_signature:
  | t = type_specifier; id = ID
    { (t, id, false) }
  | t = type_specifier; id = ID; LEFT_BRACKET; RIGHT_BRACKET
    { (t, id, true) }
  ;

parameter_types:
  | VOID_LIT
    { Ast.ArgVoid }
  | args = separated_list(COMMA, variable_signature)
    { Ast.ArgList args }
  ;

local_declaration:
  | type_specifier; ID; SEMICOLON { }
  | type_specifier; ID; LEFT_BRACKET; INT; RIGHT_BRACKET; SEMICOLON { }
  ;

declaration_list:
  | local_declaration { }
  | declaration_list; local_declaration { }
  ;

function_definition:
  | t = type_specifier; id = ID; LEFT_PAREN; args = parameter_types; RIGHT_PAREN; block = compound_statement
    { (t, id, args, Ast.Block block) }
  | VOID_LIT; id = ID; LEFT_PAREN; args = parameter_types; RIGHT_PAREN; block = compound_statement
    { (Ast.Void, id, args, Ast.Block block) }
  ;

statement:
  | compound_statement { }
  | selection_statement { }
  | expression_statement { }
  | iteration_statement { }
  | jump_statement { }
  ;

statement_list:
  | statement { }
  | statement_list; statement { }
  ;

compound_statement:
  | LEFT_BRACE; RIGHT_BRACE
    { ([], []) }
  | LEFT_BRACE; statement_list; RIGHT_BRACE
    { ([], []) }
  | LEFT_BRACE; declaration_list; RIGHT_BRACE
    { ([], []) }
  | LEFT_BRACE; declaration_list; statement_list; RIGHT_BRACE
    { ([], []) }
  ;

selection_statement:
  | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement { }
  | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement; ELSE; statement { }
  ;

expression_statement:
  | SEMICOLON { }
  | expression; SEMICOLON { }
  ;

iteration_statement:
  | WHILE; LEFT_PAREN; expression; RIGHT_PAREN; statement { }
  ;

jump_statement:
  | RETURN; SEMICOLON { }
  | RETURN; expression; SEMICOLON { }
  ;

expression:
  | conditional_expression { }
  | unary_expression; ASSIGN; expression { }
  ;

conditional_expression:
  | logical_or_expression { }
  ;

logical_or_expression:
  | logical_and_expression { }
  | logical_or_expression; L_OR; logical_and_expression { }
  ;

logical_and_expression:
  | equality_expression { }
  | logical_and_expression; L_AND; equality_expression { }
  ;

equality_expression:
  | relational_expression { }
  | equality_expression; B_EQ; relational_expression { }
  | equality_expression; B_NEQ; relational_expression { }
  ;

relational_expression:
  | additive_expression { }
  | relational_expression; B_LESS; additive_expression { }
  | relational_expression; B_GREATER; additive_expression { }
  | relational_expression; B_LEQ; additive_expression { }
  | relational_expression; B_GEQ; additive_expression { }
  ;

additive_expression:
  | multiplicative_expression { }
  | additive_expression; PLUS; multiplicative_expression { }
  | additive_expression; MINUS; multiplicative_expression { }
  ;

multiplicative_expression:
  | cast_expression { }
  | multiplicative_expression; STAR; cast_expression { }
  | multiplicative_expression; SLASH; cast_expression { }
  ;

cast_expression:
  | unary_expression { }
  | LEFT_PAREN; type_specifier; RIGHT_PAREN; cast_expression { }
  ;

unary_expression:
  | postfix_expression { }
  | unary_operator; cast_expression { }
  ;

postfix_expression:
  | primary_expression { }
  | postfix_expression; LEFT_BRACKET; expression; RIGHT_BRACKET { }
  | postfix_expression; LEFT_PAREN; separated_list(COMMA, expression); RIGHT_PAREN { }
  ;

primary_expression:
  | ID { }
  | INT { }
  | CHAR { }
  | LEFT_PAREN; expression; RIGHT_PAREN { }
  ;

unary_operator:
  | AMP { }
  | STAR { }
  | MINUS { }
  | L_NOT { }
  ;
