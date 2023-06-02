%{
/* Declarations section */
#include <stdio.h>
#include "parser.tab.hpp"
#include "hw3_output.hpp"
#include "iostream"
%}

%option yylineno
%option noyywrap


digit   		([0-9])
hexa            ([0-9A-Fa-f])
letter  		([a-zA-Z])
alphanumeric    ([a-zA-Z0-9])
whitespace		([\t\n\r ])

token_void      (void)
token_int       (int)
token_byte      (byte)
token_b         (b)
token_bool      (bool)
token_override  (override)
token_and       (and)
token_or        (or)
token_not       (not)
token_true      (true)
token_false     (false)
token_return    (return)
token_if        (if)
token_else      (else)
token_while     (while)
token_break     (break)
token_continue  (continue)
token_sc        (;)
token_comma     (,)
token_lparen    (\()
token_rparen    (\))
token_lbrace    (\{)
token_rbrace    (\})
token_assign    (=)
token_equeality    (==|!=)
token_relop     (<|>|<=|>=)
token_binop_mul     (\*|\/)
token_binop_add    (\+|\-)
token_comment   (\/\/[^\r\n]*[\r|\n|\r\n]?)
token_id        ([a-zA-Z][a-zA-Z0-9]*)
token_num       (0|[1-9]{digit}*)
token_string    (\"([^\n\r\"\\]|\\[rnt"\\])+\")

%%

{token_void}      yylval.NodeToken = (new Node_Token(yytext));return VOID;
{token_int}       yylval.NodeToken = (new Node_Token(yytext));return INT;
{token_byte}      yylval.NodeToken = (new Node_Token(yytext));return BYTE;
{token_b}         yylval.NodeToken = (new Node_Token(yytext));return B;
{token_bool}      yylval.NodeToken = (new Node_Token(yytext));return BOOL;
{token_override}  yylval.NodeToken = (new Node_Token(yytext)); return OVERRIDE;
{token_and}       yylval.NodeToken = (new Node_Token(yytext));return AND;
{token_or}        yylval.NodeToken = (new Node_Token(yytext));return OR;
{token_not}       yylval.NodeToken = (new Node_Token(yytext));return NOT;
{token_true}      yylval.NodeToken = (new Node_Token(yytext));return TRUE;
{token_false}     yylval.NodeToken = (new Node_Token(yytext));return FALSE;
{token_return}    yylval.NodeToken = (new Node_Token(yytext));return RETURN;
{token_if}        yylval.NodeToken = (new Node_Token(yytext));return IF;
{token_else}      yylval.NodeToken = (new Node_Token(yytext));return ELSE;
{token_while}     yylval.NodeToken = (new Node_Token(yytext));return WHILE;
{token_break}     yylval.NodeToken = (new Node_Token(yytext));return BREAK;
{token_continue}  yylval.NodeToken = (new Node_Token(yytext));return CONTINUE;
{token_sc}        yylval.NodeToken = (new Node_Token(yytext));return SC;
{token_lparen}    yylval.NodeToken = (new Node_Token(yytext));return LPAREN;
{token_rparen}    yylval.NodeToken = (new Node_Token(yytext));return RPAREN;
{token_lbrace}    yylval.NodeToken = (new Node_Token(yytext));return LBRACE;
{token_rbrace}    yylval.NodeToken = (new Node_Token(yytext));return RBRACE;
{token_assign}    yylval.NodeToken = (new Node_Token(yytext));return ASSIGN;
{token_relop}     yylval.NodeToken = (new Node_Token(yytext));return RELOP;
{token_comma}     yylval.NodeToken = (new Node_Token(yytext));return COMMA;
{token_equeality}    yylval.NodeToken = (new Node_Token(yytext));return EQUALITY;
{token_binop_add}     yylval.NodeToken = (new Node_Token(yytext));return BINOP_ADD;
{token_binop_mul}     yylval.NodeToken = (new Node_Token(yytext));return BINOP_MUL;

{token_id}        {yylval.NodeToken = (new Node_Token(yytext));};return ID;
{token_num}       yylval.NodeToken = (new Node_Token(yytext));return NUM;
{token_string}    {yylval.NodeToken = (new Node_Token(yytext));};return STRING;

{token_comment}              ;
{whitespace}                 ;
.		output::errorLex(yylineno);exit(0);

%%