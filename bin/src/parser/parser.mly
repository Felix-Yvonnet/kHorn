 /* File parser.mly */
%token <bool> TRUE FALSE
%token OU ET
%token LPAREN RPAREN
%token EOL
%left OU
%left ET
%start main
%type <bool> main
%%
main:
    expr EOL                { $1 }
;
expr:
     TRUE                     { $1 }
    | FALSE                   { $1 }
    | LPAREN expr RPAREN      { $2 }
    | expr OU expr            { $1 || $3 }
    | expr ET expr         { $1 && $3 }
;
