%token <int> INT
%token <char> CHAR
%token VOID_LIT
%token CHAR_LIT
%token INT_LIT
%token IF
%token ELSE
%token WHILE
%token RETURN
%token <string> ID
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token STAR
%token PLUS
%token MINUS
%token SLASH
%token AMP
%token ASSIGN
%token L_AND
%token L_OR
%token L_NOT
%token B_EQ
%token B_NEQ
%token B_LEQ
%token B_LESS
%token B_GEQ
%token B_GREATER
%token EOF

%start <unit> prog
%%

prog:
  | piece*; EOF { }
  ;

piece:
  | decl; SEMICOLON     { }
  | func                { }
  ;

decl:
  | type_decl; var_decl { }
  | type_decl; ID; LEFT_PAREN; param_types; RIGHT_PAREN { }
  | VOID_LIT; ID; LEFT_PAREN; param_types; RIGHT_PAREN { }
  ;

array_decl:
  | LEFT_BRACKET; INT; RIGHT_BRACKET { }
  ;

var_decl:
  | ID; array_decl? { }
  ;

type_decl:
  | CHAR_LIT; STAR* { }
  | INT_LIT; STAR* { }
  ;

brackets:
  | LEFT_BRACKET; RIGHT_BRACKET { }
  ;

var_sig:
  | type_decl; ID; brackets? { }

param_types:
  | VOID_LIT { }
  | separated_list(COMMA, var_sig) { }
  ;

local_decl:
  | type_decl; var_decl; SEMICOLON { }
  ;

func:
  | type_decl; ID; LEFT_PAREN; param_types; RIGHT_PAREN; LEFT_BRACE; local_decl*; stmt*; RIGHT_BRACE { }
  | VOID_LIT; ID; LEFT_PAREN; param_types; RIGHT_PAREN; LEFT_BRACE; local_decl*; stmt*; RIGHT_BRACE { }
  ;

stmt:
  | IF; LEFT_PAREN; expr; RIGHT_PAREN; stmt { }
  | IF; LEFT_PAREN; expr; RIGHT_PAREN; stmt; ELSE; stmt { }
  | WHILE; LEFT_PAREN; expr; RIGHT_PAREN; stmt { }
  | RETURN; SEMICOLON { }
  | RETURN; expr; SEMICOLON { }
  | assignment; SEMICOLON { }
  | ID; LEFT_PAREN; separated_list(COMMA, expr); RIGHT_PAREN; SEMICOLON { }
  | LEFT_BRACE; stmt*; RIGHT_BRACE { }
  | SEMICOLON { }
  ;

assignment:
  | STAR*; ID; ASSIGN; expr { }
  | STAR*; ID; LEFT_BRACKET; expr; RIGHT_BRACKET; ASSIGN; expr { }
  ;

expr:
  | MINUS; expr { }
  | L_NOT; expr { }
  | expr; binop; expr { }
  | expr; relop; expr { }
  | expr; logical_op; expr { }
  | ID { }
  | ID; LEFT_BRACKET; expr; RIGHT_BRACKET { }
  | AMP; ID { }
  | AMP; ID; LEFT_BRACKET; expr; RIGHT_BRACKET { }
  | ID; LEFT_PAREN; separated_list(COMMA, expr); RIGHT_PAREN { }
  | LEFT_PAREN; expr; RIGHT_PAREN { }
  | INT { }
  | CHAR { }
  ;

binop:
  | PLUS { }
  | MINUS { }
  | STAR { }
  | SLASH { }
  ;

relop:
  | B_EQ { }
  | B_NEQ { }
  | B_LEQ { }
  | B_LESS { }
  | B_GEQ { }
  | B_GREATER { }
  ;

logical_op:
  | L_AND { }
  | L_OR { }
  ;
