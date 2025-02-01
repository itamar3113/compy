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
    char hexString[3] = {c1, c2, '\0'};
    int asciiValue = (int)strtol(hexString, NULL, 16);
    return (asciiValue >= 0x20 && asciiValue <= 0x7E);
}

char *handleUndef(char *text, int len)
{
    char *res = new char[4];
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
    char *err = new char[4];
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
                res << '\t';
            else if (next == '\\')
                res << '\\';
            else if (next == '"')
            {
                if (i + 1 == len - 1)
                {
                    output::errorUnclosedString();
                }
                res << '"';
            }
            else if (next == '0')
            {
                break;
            }
            else if (next == 'x')
            {
                if (i + 2 == len - 1)
                {
                    err[0] = next;
                    err[1] = '\0';
                    output::errorUndefinedEscape(err);
                }
                if (i + 3 == len - 1)
                {
                    err[0] = next;
                    err[1] = text[i + 2];
                    err[2] = '\0';
                    output::errorUndefinedEscape(err);
                }
                if (!isPrintable(text[i + 2], text[i + 3]))
                {
                    err[0] = next;
                    err[1] = text[i + 2];
                    err[2] = text[i + 3];
                    err[3] = '\0';
                    output::errorUndefinedEscape(err);
                }
                char hexString[3] = {text[i + 2], text[i + 3], '\0'};
                int asciiValue = (int)strtol(hexString, NULL, 16);
                res << char(asciiValue);
                i += 2;
            }
            else
            {
                err[0] = next;
                err[1] = '\0';
                output::errorUndefinedEscape(err);
            }
            i++;
        }
        else
        {
            res << text[i];
        }
    }
    std::cout << lineo << " STRING " << res.str() << std::endl;
    delete err;
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