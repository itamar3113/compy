%{

/* Declarations section */
#include <stdio.h>
#include "tokens.hpp"

%}

%option yylineno
%option noyywrap
digit   		([0-9])
letter  		([a-zA-Z])
whitespace		([\t\n\r ])
relop           (==|!=|<|<=|>=)
binop           (\+|\-|\*|\/)
comment         (\/\/[^\n\r]*)
id              ({letter}({letter}|{digit})*)
num             (0|([1-9]{digit}*))
hexa            ([\\x20-\\x7E])
escapes         (\\\\|\\\"|\\n|\\r|\\t|\\0|{hexa})
unclosed_string (\"([^\n\r\\\"]*|[^\n\r\\\"]+([^\n\r\\\"]|{escapes})))
unopen_string   (([^\n\r\\\"]|{escapes})*\")
string          ({unclosed_string}\")
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
=         return ASSIGN;
{relop}   return RELOP;
{binop}   return BINOP;
{comment} return COMMENT;
{id}      return ID;
{num}     return NUM;
{num}b    return NUM_B;
{string}  return STRING;
{unclosed_string} return UNCLOSED_STRING;
{undef_escape}    return UNDEFINED_ESCAPE;
{whitespace} ;
{any}     return UNKNOWN_CHAR;
%%

