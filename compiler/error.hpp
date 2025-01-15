#ifndef __ERROR_HPP__
#define __ERROR_HPP__

#include <cstdio>   // For vsnprintf
#include <cstdarg> // For va_...

extern int __errno;
extern int yylineno;
extern char *yytext;
extern const char *src; // TODO!

enum ERROR_STATE {WARNING, FATAL, INFO, TYPECHECKER};

#define RESET   "\033[0m"
#define RED     "\033[31m"
#define YELLOW  "\033[33m"
#define GREEN   "\033[32m"
#define BLUE    "\033[34m"
#define VIOLET "\033[38;2;138;43;226m" 

inline void yyerror(const char *fmt, ERROR_STATE st = FATAL, int lineno = -1, ...) {
    va_list args;
    switch(st) {
        case(FATAL):
            __errno++;
            va_start(args, fmt);
            if (lineno >= 0) fprintf(stderr, "%s: line %d: " RED "Error: " RESET, src, lineno);
            else fprintf(stderr, "%s: line %d: " RED "Error near '%s': " RESET, src, yylineno, yytext);
            vfprintf(stderr, fmt, args);
            va_end(args);
            break;
            //exit(1);

        case(WARNING):
            va_start(args, fmt);
            if (lineno >= 0) fprintf(stderr, "%s: line %d: " YELLOW "Warning: " RESET, src, lineno);
            else fprintf(stderr, "%s: " YELLOW "Warning: " RESET, src);
            vfprintf(stderr, fmt, args);
            va_end(args);
            break;
        
        case(TYPECHECKER): 
            const char *expected_type, *actual_type;
            va_start(args, fmt);
            expected_type = va_arg(args, const char *);
            actual_type = va_arg(args, const char *);

            fprintf(stderr, "%s: line %d: " RED "Error: " RESET 
                    "Expected type " BLUE "'%s'" RESET " but got type " BLUE "'%s'" RESET ".\n", 
                    src, lineno, expected_type, actual_type);
            
            va_end(args);
            break;


        case (INFO):
            bool succ;
            va_start(args, fmt);
            succ = va_arg(args, int);
            if(succ) fprintf(stderr, GREEN);
            else fprintf(stderr, VIOLET);
            vfprintf(stderr, fmt, args);
            fprintf(stderr, RESET);
            va_end(args);
            break;
        default : break;
    }    
}

#endif