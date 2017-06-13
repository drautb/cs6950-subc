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

%start <Ast.ast> program
%%

program:
  | l = declaration_or_function*; EOF
    { Ast.Ast l }
  ;

declaration_or_function:
  | d = declaration; SEMICOLON
    { Ast.Declaration d }
  | f = function_definition
    { f }
  ;

declaration:
  | t = type_specifier; id = ID
    { Ast.VariableDeclaration (t, id) }
  | t = type_specifier; id = ID; LEFT_BRACKET; size = INT; RIGHT_BRACKET
    { Ast.ArrayDeclaration (t, id, size) }
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
  | t = type_specifier; id = ID; SEMICOLON
    { Ast.VariableDeclaration (t, id) }
  | t = type_specifier; id = ID; LEFT_BRACKET; size = INT; RIGHT_BRACKET; SEMICOLON
    { Ast.ArrayDeclaration (t, id, size) }
  ;

declaration_list:
  | decl = local_declaration
    { [decl] }
  | lst = declaration_list; decl = local_declaration
    { lst @ [decl] }
  ;

function_definition:
  | t = type_specifier; id = ID; LEFT_PAREN; args = parameter_types; RIGHT_PAREN; block = compound_statement
    { Ast.FunctionDefinition (t, id, args, block) }
  | VOID_LIT; id = ID; LEFT_PAREN; args = parameter_types; RIGHT_PAREN; block = compound_statement
    { Ast.FunctionDefinition (Ast.Void, id, args, block) }
  ;

statement:
  | s = compound_statement
    { s }
  | s = selection_statement
    { s }
  | s = expression_statement
    { s }
  | s = iteration_statement
    { s }
  | s = jump_statement
    { s }
  ;

statement_list:
  | stmt = statement
    { [stmt] }
  | lst = statement_list; stmt = statement
    { lst @ [stmt] }
  ;

compound_statement:
  | LEFT_BRACE; RIGHT_BRACE
    { Ast.Block ([], []) }
  | LEFT_BRACE; stmts = statement_list; RIGHT_BRACE
    { Ast.Block ([], stmts) }
  | LEFT_BRACE; decls = declaration_list; RIGHT_BRACE
    { Ast.Block (decls, []) }
  | LEFT_BRACE; decls = declaration_list; stmts = statement_list; RIGHT_BRACE
    { Ast.Block (decls, stmts) }
  ;

selection_statement:
  | IF; LEFT_PAREN; e = expression; RIGHT_PAREN; s = statement
    { Ast.Conditional (e, s, None) }
  | IF; LEFT_PAREN; e = expression; RIGHT_PAREN; s1 = statement; ELSE; s2 = statement
    { Ast.Conditional (e, s1, Some s2) }
  ;

expression_statement:
  | SEMICOLON
    { Ast.StmtVoid }
  | e = expression; SEMICOLON
    { Ast.Expression e }
  ;

iteration_statement:
  | WHILE; LEFT_PAREN; e = expression; RIGHT_PAREN; s = statement
    { Ast.Loop (e, s) }
  ;

jump_statement:
  | RETURN; SEMICOLON
    { Ast.Return None }
  | RETURN; e = expression; SEMICOLON
    { Ast.Return (Some e) }
  ;

expression:
  | e = logical_or_expression
    { e }
  | lhs = unary_expression; ASSIGN; rhs = expression
    { Ast.Assignment (lhs, rhs) }
  ;

logical_or_expression:
  | e = logical_and_expression
    { e }
  | lhs = logical_or_expression; L_OR; rhs = logical_and_expression
    { Ast.LogicalOr (lhs, rhs) }
  ;

logical_and_expression:
  | e = equality_expression
    { e }
  | lhs = logical_and_expression; L_AND; rhs = equality_expression
    { Ast.LogicalAnd (lhs, rhs) }
  ;

equality_expression:
  | e = relational_expression
    { e }
  | lhs = equality_expression; B_EQ; rhs = relational_expression
    { Ast.Equal (lhs, rhs) }
  | lhs = equality_expression; B_NEQ; rhs = relational_expression
    { Ast.NotEqual (lhs, rhs) }
  ;

relational_expression:
  | e = additive_expression
    { e }
  | lhs = relational_expression; B_LESS; rhs = additive_expression
    { Ast.LessThan (lhs, rhs) }
  | lhs = relational_expression; B_GREATER; rhs = additive_expression
    { Ast.GreaterThan (lhs, rhs) }
  | lhs = relational_expression; B_LEQ; rhs = additive_expression
    { Ast.LessThanEqual (lhs, rhs) }
  | lhs = relational_expression; B_GEQ; rhs = additive_expression
    { Ast.GreaterThanEqual (lhs, rhs) }
  ;

additive_expression:
  | e = multiplicative_expression
    { e }
  | lhs = additive_expression; PLUS; rhs = multiplicative_expression
    { Ast.Add (lhs, rhs) }
  | lhs = additive_expression; MINUS; rhs = multiplicative_expression
    { Ast.Subtract (lhs, rhs) }
  ;

multiplicative_expression:
  | e = cast_expression
    { e }
  | lhs = multiplicative_expression; STAR; rhs = cast_expression
    { Ast.Multiply (lhs, rhs) }
  | lhs = multiplicative_expression; SLASH; rhs = cast_expression
    { Ast.Divide (lhs, rhs) }
  ;

cast_expression:
  | e = unary_expression
    { e }
  | LEFT_PAREN; t = type_specifier; RIGHT_PAREN; e = cast_expression
    { Ast.Cast (t, e) }
  ;

unary_expression:
  | e = postfix_expression
    { e }
  | AMP; e = cast_expression
    { Ast.AddressOf e }
  | STAR; e = cast_expression
    { Ast.Dereference e }
  | MINUS; e = cast_expression
    { Ast.Negate e }
  | L_NOT; e = cast_expression
    { Ast.LogicalNot e }
  ;

postfix_expression:
  | e = primary_expression
    { e }
  | arr = postfix_expression; LEFT_BRACKET; idx = expression; RIGHT_BRACKET
    { Ast.ArrayRef (arr, idx) }
  | fn = postfix_expression; LEFT_PAREN; args = separated_list(COMMA, expression); RIGHT_PAREN
    { Ast.FunctionCall (fn, args) }
  ;

primary_expression:
  | id = ID
    { Ast.Id id }
  | n = INT
    { Ast.IntConst n }
  | c = CHAR
    { Ast.CharConst c }
  | LEFT_PAREN; e =  expression; RIGHT_PAREN
    { e }
  ;
