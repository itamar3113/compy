#include "tokens.hpp"
#include "output.hpp"
#include <sstream>
#include <iostream>

bool isValidHexDigit(char c)
{
    return (c >= '0' && c <= '9') ||
           (c >= 'A' && c <= 'F') ||
           (c >= 'a' && c <= 'f');
}

bool isPrintable(char c1, char c2)
{
    if (!isValidHexDigit(c1) || !isValidHexDigit(c2))
    {
        return false;
    }
    unsigned short value = (unsigned char)c1 << 8 | (unsigned char)c2;
    return (value >= 0x20 && value <= 0x7E);
}

char *handleUndef(char *text, int len)
{
    char* res = new char[4];
    for (int i = 0; i < len; i++)
    {
        if (text[i] == '\\')
        {
            char next = text[i + 1];
            if (next != '\\' && next != 'n' && next != 'r' && next != 't' && next != '0' && next != 'x')
            {
                res[0] = next;
                res[1] = '\0';
                return res;
            }
            else if (next == 'x')
            {
                if (text[i + 2] == '\"')
                {
                    res[0] = next;
                    res[1] = '\0';
                    return res;
                }
                else if (text[i + 3] == '\"')
                {
                    res[0] = next;
                    res[1] = text[i + 2];
                    res[2] = '\0';
                    return res;
                }
                else if (!isPrintable(text[i + 2], text[i + 3]))
                {
                    res[0] = next;
                    res[1] = text[i + 2];
                    res[2] = text[i + 3];
                    res[3] = '\0';
                    return res;
                }
            }
        }
    }
    return nullptr;
}

void printString(int lineo, char *text, int len)
{
    std::ostringstream res;
    for (int i = 1; i < len - 1; i++)
    {
        if (text[i] == '\\')
        {
            char next = text[i + 1];
            if (next == 'n')
                res << '\n';
            else if (next == 'r')
                res << '\r';
            else if (next == 't')
                res << '\r';
            else if (next == '\\')
                res << '\\';
            else if (next == '"')
                res << '"';
            else if (next == 'x')
            {
                int val = text[i + 2];
                char hexa_char = (val << 4) | text[i + 3];
                res << hexa_char;
            }
        }
        else
        {
            res << text[i];
        }
    }
    std::cout << lineo << " STRING " << res.str() << std::endl;
}

int main()
{
    enum tokentype token;

    // read tokens until the end of file is reached
    while ((token = static_cast<tokentype>(yylex())))
    {
        switch (token)
        {
        case UNKNOWN_CHAR:
            output::errorUnknownChar(yytext[0]);

        case UNDEFINED_ESCAPE:
            output::errorUndefinedEscape(handleUndef(yytext, yyleng));

        case UNCLOSED_STRING:
            output::errorUnclosedString();

        case STRING:
            printString(yylineno, yytext, yyleng);
            break;

        default:
            output::printToken(yylineno, token, yytext);
        }
    }
    return 0;
}