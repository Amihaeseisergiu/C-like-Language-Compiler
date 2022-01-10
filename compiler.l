%{
#include <stdio.h>
#include "y.tab.h"
%}
%%
"int" {return INT;}
"float" {return FLOAT;}
"char" {return CHAR;}
"string" {return STRING;}
"bool" {return BOOL;}
"true" {return TRUE;}
"false" {return FALSE;}
"main" {return MAIN;}
"for" {return FOR;}
"if" {return IF;}
"else" {return ELSE;}
"while" {return WHILE;}
"const" {return CONST;}
"container" {return CONTAINER;}
"Eval" {return EVAL;}
"return" {return RETURN;}
"=" {return ATR;}
"and" {return AND;}
"or" {return OR;}
"==" {return EQEQ;}
"!=" {return NEQ;}
">=" {return GEQ;}
"<=" {return LEQ;}
">" {return GT;}
"<" {return LW;}
"!" {return NOT;}
"+" {return PLS;}
"-" {return MIN;}
"*" {return MUL;}
"/" {return DIV;}
"%" {return MOD;}
\"[^\"]+\" {yytext = yytext+1; yytext[strlen(yytext)-1] = '\0'; yylval.stringval = strdup(yytext); return STRINGVAL;}
\'[^\']\' {yylval.charval = yytext[1]; return CHARVAL;}
[0-9]+\.[0-9]+  {yylval.floatval = atof(yytext); return FLOATVAL; }
[0-9]+ {yylval.intval = atoi(yytext); return NR;}
[_a-zA-Z][_a-zA-Z0-9]* {yylval.stringval = strdup(yytext); return ID;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}
