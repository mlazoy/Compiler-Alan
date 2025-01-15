%{
#include <cstdlib>

#include "lexer.hpp"
#include "ast.hpp"
#include "parser.hpp"

#define T_EOF 0

int nested_comment = 0;

extern int yylineno; //default Flex line tracker

%}

L [A-Za-z]
D [0-9]
H [0-9a-fA-F]
W [ \t\r\0]
E \\([\"\\n\'tr0])

%x COMMENT

%option noyywrap 

%% 

\n { ++yylineno;}

"byte" {return T_byte;}
"return" {return T_return;}
"else" {return T_else;}
"while" {return T_while;}
"false" {return T_false;}
"true" {return T_true;}
"if" {return T_if;}
"int" {return T_int;}
"proc" {return T_proc;}
"reference" {return T_reference;}


"=="                                    {yylval.op = 'e'; return T_equal;}
"!="                                    {yylval.op = 'n'; return T_not_equal;}
">="                                    {yylval.op = 'g'; return T_greater_or_eq;}
"<="                                    {yylval.op = 'l'; return T_less_or_eq;}

[\=\+\-\*\/\%\!\&\|\<\>] {yylval.op = yytext[0]; return yytext[0];} /*single-char operators*/
[\(\)\[\]\{\}\,\:\;] {yylval.op = yytext[0]; return yytext[0];}     /*discriminators*/

{D}+                                    {yylval.num = atoi(yytext); return T_const;}
{L}({L}|{D}|"_")*                       {yylval.name = strdup(yytext); return T_id;}

\'([^\"\\\n\']|{E})\'|\'\\x{H}{H}?\'     {yylval.byte = getAscii(yytext); return T_char; }

\"([^\"\\\n]|\\[\"\\ntr0])*\"            {  yylval.name = strdup(yytext);
                                            return T_string;
                                        }

"--".* { /*ignore single line comments*/ }

"(*" { nested_comment = 1;
      BEGIN(COMMENT);
      }

<COMMENT>{
"(*" { ++nested_comment;} 
"*)" {
    if (--nested_comment == 0) {
        BEGIN(INITIAL); /*exit comment state */
    }
}
\n { ++yylineno;}
. {/* ignore */}
}

{W}+ {/*eat them up*/}

.   { yyerror("Illegal character '%c'.\n", FATAL, yylineno, yytext[0]); }

%%
