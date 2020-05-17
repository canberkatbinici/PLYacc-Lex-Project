%{
#include <stdlib.h>
#include "leaf.h"
#include "y.tab.h"
void yyerror(char *);
%}

%%
">>>"		{return COMMENTLINE;}
">>|"		{return OPENCOMMENT;}
"|>>"		{return CLOSECOMMENT;}
"WHITESPACE"	;
"TRUE"          {return TRUE;}
"FALSE"         {return FALSE;}
"BOOL"   	{return BOOLEAN;}
"FUNCTION"	{return FUNCTION;}
"ENDF"		{return END_FUNCTION;}
"RETURN"	{return RETURN;}
"IF"		{return IF;}
"ELSE"		{return ELSE;}
"ELF"		{return ELSE_IF;}
"DO"		{return DO;}
"WHILE"		{return WHILE;}
"LOOPW"		{return END_WHILE;}
"FOR"		{return FOR;}
"LOOPF"		{return END_FOR;}
"TRY"		{return TRY;}
"CATCH"		{return CATCH;}
"THEN"		{return THEN;}
"SWITCH"	{return SWITCH;}
"CASE"		{return CASE;}
"IMPORT"	{return IMPORT;}
"STOP"		{return BREAK;}
"CONTINUE"	{return CONTINUE;}
"PRINTLOG"	{return CONSOLPRINT;}
"SCANLOG"	{return CONSOLSCANNER;}
"NL"		{return NL;}
"^"             {return POW;}
"+"             {return PLUS;}
"-"             {return MINUS;}
"*"             {return MULTIPLE;}
"/"             {return DIVIDE;}
"%"             {return MOD;}
"<"             {return LESSTHAN;}
">"             {return GREATERTHAN;}
">+"		{return INCREASE;}
">-"		{return DECREASE;}
":"		{return COLON;}
";"		{return SEMICOLON;}
","		{return COMMA;}
">="		{return EQUALORGREAT;}
"<="	        {return EQUALORLESS;}
"=="		{return EQUAL;}
"!=!"		{return ISNOTEQUAL;}
"&&"		{return AND;}
"><"		{return OR;}
"("		{return OPENBR;}
")"		{return CLOSEBR;}
"{"		{return OPENCBR;}
"}"		{return CLOSECBR;}
\[		{return OPENSBR;}
\]		{return CLOSESBR;}
\"		{return QUOTES;}



0                           {yylval.integer = atoi(yytext); return INT; }
[1-9][0-9]*                 {yylval.integer = atoi(yytext); return INT; }
[a-zA-Z_]                   { yylval.ide = *yytext - 'a'; return IDENTIFIER; }
[-()<>=+*/;{}.]             {return *yytext; }
[+-]?[0-9]+[\.][0-9]*	    {return DINT;}
^\d*\.?\d*$	            {return FLOAT;}
[ \t\n]		             ;
.		            yyerror("Unknown character");
%%

int yywrap (void) {return 1;}
