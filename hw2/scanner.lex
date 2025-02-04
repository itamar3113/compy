%{

/* Declarations section */
#include <stdio.h>
#include "nodes.hpp"
#include "parser.tab.h"
#include "output.hpp"
%}

%option yylineno
%option noyywrap
digit   		([0-9])
letter  		([a-zA-Z])
whitespace		([\t\n\r ])
relop           (==|!=|<|>|<=|>=)
binop           (\+|\-|\*|\/)
comment         (\/\/[^\n\r]*)
id              ({letter}({letter}|{digit})*)
num             (0|([1-9]{digit}*))
hexa            ([\\x20-\\x7E])
escapes         (\\\\|\\\"|\\n|\\r|\\t|\\0|{hexa})
unclosed_string (\"([^\n\r\"\\]|{escapes})*)
unopen_string   (([^\n\r\\\"]|{escapes})*\")
string          (\"([^\n\r\"\\]|\\[rnt"\\])+\")
undef_escape    ({unclosed_string}\\([^ntr0"]|x))
any             (.)
%%
void      return VOID;
int       return INT;
byte      return BYTE;
bool      return BOOL;
and       return AND;
or        return OR;
not       return NOT;
true      return TRUE;
false     return FALSE;
return    return RETURN;
if        return IF;
else      return ELSE;
while     return WHILE;
break     return BREAK;
continue  return CONTINUE;
;         return SC;
,         return COMMA;
\(        return LPAREN;
\)        return RPAREN;
\{        return LBRACE;
\}        return RBRACE;
=         return ASSIGN;
\+        return PLUS; 
\-        return MINUS; 
\*        return TIMES; 
\/        return DIVIDE;
\<         return LT_OP; 
\>         return GT_OP;
"=="      return EQ_OP;
"!="      return NE_OP;
"<="      return LE_OP;
">="      return GE_OP;
{id}      {yylval = std::make_shared<ast::ID>(yytext); return ID; } 
{num}     {yylval = std::make_shared<ast::Num>(yytext); return NUM; }
{num}b    {yylval = std::make_shared<ast::NumB>(yytext); return NUMB; }
{string}  {yylval = std::make_shared<ast::String>(yytext); return STRING; }
{whitespace} ;
{any}     {output::errorLex(yylineno);
            exit(0);
          };
%%

