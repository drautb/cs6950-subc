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

%start <unit> program
%%

program:
  | declaration_or_function*; EOF { }
  ;

declaration_or_function:
  | declaration; SEMICOLON { }
  | function_declaration { }
  ;

declaration:
  | type_specifier; variable_declaration { }
  | type_specifier; ID; LEFT_PAREN; parameter_types; RIGHT_PAREN { }
  | VOID_LIT; ID; LEFT_PAREN; parameter_types; RIGHT_PAREN { }
  ;

array_declaration:
  | LEFT_BRACKET; INT; RIGHT_BRACKET { }
  ;

variable_declaration:
  | ID; array_declaration? { }
  ;

type_specifier:
  | CHAR_LIT; STAR* { }
  | INT_LIT; STAR* { }
  ;

array_signature:
  | LEFT_BRACKET; RIGHT_BRACKET { }
  ;

variable_signature:
  | type_specifier; ID; array_signature? { }

parameter_types:
  | VOID_LIT { }
  | separated_list(COMMA, variable_signature) { }
  ;

local_declaration:
  | type_specifier; variable_declaration; SEMICOLON { }
  ;

function_declaration:
  | type_specifier; ID; LEFT_PAREN; parameter_types; RIGHT_PAREN; LEFT_BRACE; local_declaration*; statement*; RIGHT_BRACE { }
  | VOID_LIT; ID; LEFT_PAREN; parameter_types; RIGHT_PAREN; LEFT_BRACE; local_declaration*; statement*; RIGHT_BRACE { }
  ;

statement:
  | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement { }
  | IF; LEFT_PAREN; expression; RIGHT_PAREN; statement; ELSE; statement { }
  | WHILE; LEFT_PAREN; expression; RIGHT_PAREN; statement { }
  | RETURN; SEMICOLON { }
  | RETURN; expression; SEMICOLON { }
  | assignment; SEMICOLON { }
  | ID; LEFT_PAREN; separated_list(COMMA, expression); RIGHT_PAREN; SEMICOLON { }
  | LEFT_BRACE; statement*; RIGHT_BRACE { }
  | SEMICOLON { }
  ;

assignment:
  | STAR*; ID; ASSIGN; expression { }
  | STAR*; ID; LEFT_BRACKET; expression; RIGHT_BRACKET; ASSIGN; expression { }
  ;

expression:
  | MINUS; expression { }
  | L_NOT; expression { }
  | expression; binary_op; expression { }
  | expression; relational_op; expression { }
  | expression; logical_op; expression { }
  | ID { }
  | ID; LEFT_BRACKET; expression; RIGHT_BRACKET { }
  | AMP; ID { }
  | AMP; ID; LEFT_BRACKET; expression; RIGHT_BRACKET { }
  | ID; LEFT_PAREN; separated_list(COMMA, expression); RIGHT_PAREN { }
  | LEFT_PAREN; expression; RIGHT_PAREN { }
  | INT { }
  | CHAR { }
  ;

binary_op:
  | PLUS { }
  | MINUS { }
  | STAR { }
  | SLASH { }
  ;

relational_op:
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
