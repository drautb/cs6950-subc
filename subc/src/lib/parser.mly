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

declaration_list:
  | local_declaration { }
  | declaration_list; local_declaration { }
  ;

function_declaration:
  | type_specifier; ID; LEFT_PAREN; parameter_types; RIGHT_PAREN; compound_statement { }
  | VOID_LIT; ID; LEFT_PAREN; parameter_types; RIGHT_PAREN; compound_statement { }
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
  | LEFT_BRACE; RIGHT_BRACE { }
  | LEFT_BRACE; statement_list; RIGHT_BRACE { }
  | LEFT_BRACE; declaration_list; RIGHT_BRACE { }
  | LEFT_BRACE; declaration_list; statement_list; RIGHT_BRACE { }
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
