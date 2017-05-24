Language Design
===============

Taken in part from: https://www2.cs.arizona.edu/~debray/Teaching/CSc453/DOCS/cminusminusspec.html

## Lexical Rules

```
<letter> ::= [a-zA-Z]

<digit> ::= [0-9]

<id> ::= <letter>[<letter> | <digit>]*

<intcon> ::= <digit>+

<charcon> ::= [ -&\(-\[\]-~] | '\n' | '\0' 
  (denotes any printable ASCII character, as specified by isprint(), other than \ (backslash) and ' (single quote).
```

## Grammar Productions

```
<prog> : { <decl>';'  | <func> }

<decl> : <type_decl> <var_decl> 
       | <type_decl> <id> '(' <param_types> ')' 
       | 'void' <id> '(' <param_types> ')'

<var_decl> : <id>
           | <id>'['<intcon>']'

<type_decl> : 'char' ('*')*
            | 'int' ('*')*

<param_types> : 'void'
              | (<type_decl> <id> ('[]')? )* 

<func> : <type_decl> <id> '(' <param_types> ')' '{' { <type_decl> <var_decl>';' } { stmt } '}'
       | 'void' <id> '(' <param_types> ')' '{' { <type_decl> <var_decl>';' } { stmt } '}'

<stmt> : 'if' '(' <expr> ')' <stmt>
       | 'if' '(' <expr> ')' <stmt> 'else' <stmt>
       | 'while' '(' <expr> ')' <stmt>
       | 'return' ';'
       | 'return' <expr> ';'
       | <assignment> ';'
       | <id> '()' ';'
       | <id> '(' <expr> (',' <expr>)* ')' ';'
       | '{' (<stmt>)* '}'
       | ';'

<assignment> : ('*')* <id> = <expr>
             | ('*')* <id>'[' <expr> ']' = <expr>
       
<expr>  : '–' <expr>
        | '!' <expr>
        | <expr> <binop> <expr>
        | <expr> <relop> <expr>
        | <expr> <logical_op> <expr>
        | <id>
        | <id> '[' <expr> ']' 
        | '&' <id>
        | '&' <id> '[' <expr> ']' 
        | <id> '(' (<expr> (',' <expr>)* )* ')'
        | '(' <expr> ')'
        | <intcon>
        | <charcon>

<binop> : '+'
        | '–'
        | '*'
        | '/'

<relop> : '=='
        | '!='
        | '<='
        | '<'
        | '>='
        | '>'

<logical_op> : '&&'
             | '||'
```


Lexer Tokens
============

LETTER = [a-zA-Z]
DIGIT = [0-9]
ID = LETTER [LETTER|DIGIT]*


