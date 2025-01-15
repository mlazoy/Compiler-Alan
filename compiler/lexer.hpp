#ifndef LEXER_HPP
#define LEXER_HPP

#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <cstdio>

int yylex();

inline uint8_t getAscii(const char *yytext) {
    uint8_t ascii_val = 0;
    if (!(yytext[0]== '\'' && yytext[strlen(yytext)-1]=='\'')) {
        fprintf(stderr,"False charachter format");
        exit(1);
    }
    if (yytext[1] == '\\') {
        switch(yytext[2]) {
            case 'x' :  //hex number in the format \xnn
                ascii_val = static_cast<int>(std::strtol(yytext+3,nullptr,16));
                break;
            case 'n' : ascii_val = uint8_t('\n'); break;
            case 't' : ascii_val = uint8_t('\t'); break;
            case 'r' : ascii_val = uint8_t('\r'); break;
            case '0' : ascii_val = 0; break;
            case '\\': ascii_val = uint8_t('\\'); break;
            case '\'': ascii_val = uint8_t('\''); break;
            case '\"': ascii_val = uint8_t('\"'); break;
            default:
                fprintf(stderr, "Forbidden escape charachter!");
                //exit(1);
        }

    }

    else ascii_val = uint8_t(yytext[1]); //normal charachters
    
    return ascii_val;
}


#endif