%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <stdarg.h>
    #include "leaf.h"
    #include "y.tab.h"

    nodeType *opr(int oper, int nops, ...);
    nodeType *id(int i);
    nodeType *con(int value);
    void freeNode(nodeType *p);
    int ex(nodeType *p);
    int yylex(void);
    void yyerror(char *s);
    int sym[26];                    
%}

%union {
    int integer;                 
    char ide;                /* sembol tablosu */
    nodeType *nPtr;             
};

%token <integer> INT
%token <ide> IDENTIFIER
%token WHILE IF CONSOLPRINT  ELSE_IF END_WHILE COMMENTLINE OPENCOMMENT CLOSECOMMENT TRUE FALSE BOOLEAN STR DINT FLOAT
%token FUNCTION END_FUNCTION RETURN ELSE DO FOR END_FOR TRY CATCH THEN SWITCH CASE IMPORT BREAK CONTINUE
%token  CONSOLSCANNER POW PLUS MINUS MULTIPLE DIVIDE MOD LESSTHAN GREATERTHAN NL INCREASE DECREASE
%token COLON SEMICOLON COMMA EQUALORGREAT EQUALORLESS EQUAL ISNOTEQUAL AND OR OPENBR CLOSEBR OPENCBR CLOSECBR OPENSBR CLOSESBR QUOTES
%nonassoc IFX
%nonassoc ELSE

%left EQUALORGREAT EQUALORLESS
%left EQUAL ISNOTEQUAL 
%left GREATERTHAN LESSTHAN
%left PLUS MINUS
%left MULTIPLE DIVIDE
%nonassoc UMINUS

%type <nPtr> stmt expr stmt_list

%%

program:
        function                { exit(0); }
        ;

function:
          function stmt         { ex($2); freeNode($2); }
        |
        ;

stmt:
          SEMICOLON                      { $$ = opr(SEMICOLON, 2, NULL, NULL); }
        | expr SEMICOLON                 { $$ = $1; }
        | CONSOLPRINT expr SEMICOLON     { $$ = opr(CONSOLPRINT, 1, $2); }
        | IDENTIFIER '=' expr SEMICOLON  { $$ = opr('=', 2, id($1), $3); }
        | WHILE OPENBR expr CLOSEBR stmt END_WHILE       { $$ = opr(WHILE, 2, $3, $5, END_WHILE); } /* while op, stmt, c style while */ 
        | IF OPENBR expr CLOSEBR stmt %prec IFX { $$ = opr(IF, 2, $3, $5); }
        | IF OPENBR expr CLOSEBR stmt ELSE stmt { $$ = opr(IF, 3, $3, $5, $7); }
        | OPENCBR stmt_list CLOSECBR            { $$ = $2; }
	| Comment stmt_list			{;}
        ;


stmt_list:
          stmt                  { $$ = $1; }
        | stmt_list stmt        { $$ = opr(SEMICOLON, 2, $1, $2); }
        ;

Comment	: 				COMMENTLINE
				| OPENCOMMENT
				;

expr:
          INT                           { $$ = con($1); }
        | IDENTIFIER                    { $$ = id($1); }
        | MINUS expr %prec UMINUS       { $$ = opr(UMINUS, 1, $2); }
        | expr PLUS expr                { $$ = opr(PLUS, 2, $1, $3); }
        | expr MINUS expr               { $$ = opr(MINUS, 2, $1, $3); }
        | expr MULTIPLE expr            { $$ = opr(MULTIPLE, 2, $1, $3); }
        | expr DIVIDE expr              { $$ = opr(DIVIDE, 2, $1, $3); }
        | expr LESSTHAN expr            { $$ = opr(LESSTHAN, 2, $1, $3); }
        | expr GREATERTHAN expr         { $$ = opr(GREATERTHAN, 2, $1, $3); }
		| expr EQUALORGREAT expr        { $$ = opr(EQUALORGREAT, 2, $1, $3); }
		| expr EQUALORLESS expr          { $$ = opr(EQUALORLESS, 2, $1, $3); }
        | expr ISNOTEQUAL expr          { $$ = opr(ISNOTEQUAL, 2, $1, $3); }
        | expr EQUAL expr          { $$ = opr(EQUAL, 2, $1, $3); }
        | OPENBR expr CLOSEBR          { $$ = $2; }
        ;

%%


