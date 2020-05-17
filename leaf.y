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


nodeType *con(int value) {
    nodeType *p;


    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("out of memory");


    p->type = typeCon;
    p->con.value = value;

    return p;
}

nodeType *id(int i) {
    nodeType *p;


    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("out of memory");


    p->type = typeId;
    p->id.i = i;

    return p;
}

nodeType *opr(int oper, int nops, ...) {
    va_list ap;
    nodeType *p;
    int i;


    if ((p = malloc(sizeof(nodeType) + (nops-1) * sizeof(nodeType *))) == NULL)
        yyerror("out of memory");


    p->type = typeOpr;
    p->opr.oper = oper;
    p->opr.nops = nops;
    va_start(ap, nops);
    for (i = 0; i < nops; i++)
        p->opr.op[i] = va_arg(ap, nodeType*);
    va_end(ap);
    return p;
}

void freeNode(nodeType *p) {
    int i;

    if (!p) return;
    if (p->type == typeOpr) {
        for (i = 0; i < p->opr.nops; i++)
            freeNode(p->opr.op[i]);
    }
    free (p);
}

void yyerror(char *s) {
    fprintf(stdout, "%s\n", s);
}

int main(void) {
    yyparse();
    return 0;
}

int ex(nodeType *p) {
    if (!p) return 0;
    switch(p->type) {
    case typeCon:       return p->con.value;
    case typeId:        return sym[p->id.i];
    case typeOpr:
        switch(p->opr.oper) {
        case WHILE:     while(ex(p->opr.op[0])) ex(p->opr.op[1]); return 0;
        case IF:        if (ex(p->opr.op[0]))
                            ex(p->opr.op[1]);
                        else if (p->opr.nops > 2)
                            ex(p->opr.op[2]);
                        return 0;
        case CONSOLPRINT:     printf("%d\n", ex(p->opr.op[0])); return 0;
        case SEMICOLON:       ex(p->opr.op[0]); return ex(p->opr.op[1]);
        case '=':       return sym[p->opr.op[0]->id.i] = ex(p->opr.op[1]);
        case UMINUS:    return -ex(p->opr.op[0]);
        case PLUS:       return ex(p->opr.op[0]) + ex(p->opr.op[1]);
        case MINUS:       return ex(p->opr.op[0]) - ex(p->opr.op[1]);
        case MULTIPLE:       return ex(p->opr.op[0]) * ex(p->opr.op[1]);
        case DIVIDE:       return ex(p->opr.op[0]) / ex(p->opr.op[1]);
        case LESSTHAN:       return ex(p->opr.op[0]) < ex(p->opr.op[1]);
        case GREATERTHAN:       return ex(p->opr.op[0]) > ex(p->opr.op[1]);
        case EQUALORGREAT:        return ex(p->opr.op[0]) >= ex(p->opr.op[1]);
        case EQUALORLESS:        return ex(p->opr.op[0]) <= ex(p->opr.op[1]);
        case ISNOTEQUAL:        return ex(p->opr.op[0]) != ex(p->opr.op[1]);
        case EQUAL:        return ex(p->opr.op[0]) == ex(p->opr.op[1]);
        }
    }
    return 0;
}
