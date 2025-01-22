%error-verbose

%{
#include <cstdio> 
#include <cstdlib>
#include <string>
#include <vector>
#include <utility>
#include <string>
#include <fstream>
#include <sstream>

#include "lexer.hpp"
#include "ast.hpp"
#include "error.hpp"

SymbolTable st;
RecordTable rt;

int __errno = 0;

const char *src;
const char *inp;
bool OPT = 0;
bool DEBUG = 0;
std::string ast_file, symbol_file;
std::ofstream ast_stream, symbol_stream;

RetType ret_val;

%}

%token T_byte "byte"
%token T_return "return"
%token T_else "else"
%token T_while "while"
%token T_false "false"
%token T_true "true"
%token T_if "if"
%token T_int "int"
%token T_proc "proc"
%token T_reference "reference"

%token T_equal "=="
%token T_not_equal "!="
%token T_greater_or_eq ">="
%token T_less_or_eq "<="
%token T_eq "="
%token T_gr ">"
%token T_le "<"
%token T_not "!"
%token T_and "&"
%token T_or "|"
%token T_add "+"
%token T_minus "-"
%token T_mul "*"
%token T_div "/"
%token T_mod "%"

%token T_assign ":"
%token T_semicolon ";"
%token T_comma ","
%token T_l_par "("
%token T_r_par ")"
%token T_l_br "["
%token T_r_br "]"
%token T_l_curlbr "{"
%token T_r_curlbr "}"

%token<num> T_const 
%token<name> T_id T_string 
%token<byte> T_char

/*priorities*/

%left<op> '!'
%left<op> '+' '-'
%left<op> '*' '/' '%'
%nonassoc<op> "==" "!=" '<' '>' "<=" ">="
%left<op> '&'
%left<op> '|'

%expect 1

%union {
    int num;
    uint8_t byte;
    char *name;    /* for identifiers */
    char op;     /* single charachter operator */
    
    AST *ast;
    Stmt *stmt;
    Expr *expr;
    StmtBlock *stmt_blk;
    ExprBlock *expr_blk;
    Variable *var;
    VarBlock *var_blk;
    FunctionDecl *decl;
    FunctionCall *call;
    AlanType *type;
    BaseType base_type;
    Parameter *param;
    ParamBlock *param_blk;
    LocalDefList *locals;
    Definition *def;
    LVal *lval;
}

%type<ast> program
%type<decl> func_def
%type<call> func_call
%type<expr> expr cond
%type<stmt> stmt
%type<expr_blk> expr_list 
%type<stmt_blk> stmt_list comp_stmt 
%type<base_type> data_type
%type<type> type r_type
%type<param_blk> fpar_list
%type<param> fpar_def
%type<var> var_def
%type<locals> local_def_list 
%type<def> local_def
%type<lval> l_value

%%

program:
    func_def {
        //std::cerr << "AST: " << *$1 << std::endl;
        if (DEBUG) {
        /*  ast_file = std::string(inp) + ".ast";
            symbol_file = std::string(inp) + ".symbol";

            symbol_stream.open(symbol_file, std::ios::app);
            ast_stream.open(ast_file, std::ios::app); */

            if (! ast_stream.is_open()) 
                std::cerr <<"Error: Could not open " << ast_file << " for writing AST\n";
            else {
            ast_stream << "AST:\n";
            ast_stream << *$1;
            ast_stream.close();
            }
            if (! symbol_stream.is_open()) {
                std::cerr <<"Error: Could not open " << symbol_file << " for writing Symbol Table\n";
                DEBUG = 0; // to silent errors caused by stream
            }
        }
         st.init(); 
         $1->sem(ret_val);
         st.clear();
        if (__errno == 0) {
            rt.init(); 
            $1->llvm_cgen(OPT); 
            rt.clear();
        }
        if (DEBUG && symbol_stream.is_open())
            symbol_stream.close(); 
    }
;

func_def:   
    T_id '(' fpar_list ')' ':' r_type local_def_list comp_stmt  {$$ = new FunctionDecl($1, $3, *($6), $8, $7);}
;

fpar_list:
    /* empty */                         {$$ = new ParamBlock(); }
|   fpar_def                            {$$ = new ParamBlock(); $$->append($1);}
|   fpar_list ',' fpar_def              {$$ = $1; $$->append($3);}
;

fpar_def:  
    T_id ':' type                       {$$ = new Parameter($1, false, *($3));}
|   T_id ':' "reference" type           {$$ = new Parameter($1, true, *($4));}
;

data_type:
    "int"                               {$$ = INT;}
|   "byte"                              {$$ = BYTE;}
;

type:
    data_type                           {$$ = new AlanType($1);}
|   data_type '[' ']'                   {$$ = new AlanType(ARRAY, new AlanType($1));}
;

r_type:
    data_type                           {$$ = new AlanType($1);}
|   "proc"                              {$$ = new AlanType(PROC);}
;

local_def_list:  
    /* empty */                         {$$ = new LocalDefList();}
|   local_def_list local_def            {$$->append_local_def($2); $$ = $1;}
;

local_def: 
    func_def                            {$$ = $1;}
|   var_def                             {$$ = $1;}
;

var_def:    
    T_id ':' data_type ';'                 {$$ = new Variable($1, $3);}
|   T_id ':' data_type '[' T_const ']' ';' {$$ = new Array($1, $3, $5);}
;

stmt: 
    ';'                                     { /*nothing*/ }
|   l_value '=' expr ';'                    {$$ = new Assign($1, $3);}
|   comp_stmt                               {$$ = $1;}
|   func_call ';'                           {$$ = $1;}
|   "if" '(' cond ')' stmt                  {$$ = new If($3, $5);}
|   "if" '(' cond ')' stmt "else" stmt      {$$ = new If($3, $5, $7);}   
|   "while" '(' cond ')' stmt               {$$ = new While($3, $5);}
|   "return" ';'                            {$$ = new Return();}
|   "return" expr ';'                       {$$ = new Return($2);}
;

comp_stmt: 
    '{' stmt_list '}'                       {$$ = $2;}
;

stmt_list:
    /* empty */                             {$$ = new StmtBlock();}
|   stmt_list stmt                          {$$->append($2);}
;

func_call:  
    T_id '(' ')'                            {$$ = new FunctionCall($1);}
|   T_id '(' expr_list ')'                  {$$ = new FunctionCall($1, $3);}
;

expr_list:
    expr                                    {$$ = new ExprBlock(); $$->append($1);}
|   expr_list ',' expr                      {$$->append($3);}
;

expr:
    T_const                                 {$$ = new IntConst($1);}
|   T_char                                  {$$ = new ByteConst($1);}
|   l_value                                 {$$ = $1;} 
|   '(' expr ')'                            {$$ = $2;}
|   func_call                               {$$ = $1;}
|   '+' expr                                {$$ = new UnaryExpr($1, $2);}
|   '-' expr                                {$$ = new UnaryExpr($1, $2);}
|   expr '+' expr                           {$$ = new BinaryExpr($2, $1, $3);}
|   expr '-' expr                           {$$ = new BinaryExpr($2, $1, $3);}
|   expr '*' expr                           {$$ = new BinaryExpr($2, $1, $3);}
|   expr '/' expr                           {$$ = new BinaryExpr($2, $1, $3);}
|   expr '%' expr                           {$$ = new BinaryExpr($2, $1, $3);}
;

l_value: 
    T_id                                    {$$ = new Id($1);}
|   T_id '[' expr ']'                       {$$ = new ArrayItem($1, $3);}
|   T_string                                {$$ = new StringConst($1);}              
;

cond:
    "true"                                  {$$ = new BoolConst(true);}
|   "false"                                 {$$ = new BoolConst(false);}
|   '(' cond ')'                            {$$ = $2;}
|   '!' cond                                {$$ = new UnaryExpr($1, $2);}
|   expr "==" expr                          {$$ = new BinaryExpr($2, $1, $3);}
|   expr "!=" expr                          {$$ = new BinaryExpr($2, $1, $3);}
|   expr '<' expr                           {$$ = new BinaryExpr($2, $1, $3);}
|   expr '>' expr                           {$$ = new BinaryExpr($2, $1, $3);}
|   expr ">=" expr                          {$$ = new BinaryExpr($2, $1, $3);}
|   expr "<=" expr                          {$$ = new BinaryExpr($2, $1, $3);}
|   cond '&' cond                           {$$ = new BinaryExpr($2, $1, $3);} 
|   cond '|' cond                           {$$ = new BinaryExpr($2, $1, $3);}
;

%%

int main(int argc, char *argv[]) {
    int result = 0;
    // first parameter is sourcefile
    inp = argv[1];
    src = (std::string(inp) + ".alan").c_str();
    // Check for flags
    for (int i = 2; i < argc; ++i) {
        if (strcmp(argv[i], "-O") == 0)
            OPT = true; 
        if (strcmp(argv[i], "-d") == 0) {
            DEBUG = true;
            ast_file = std::string(inp) + ".ast";
            symbol_file = std::string(inp) + ".symbol";

            //truncate them if needed first
            std::ofstream(ast_file, std::ios::out).close();
            std::ofstream(symbol_file, std::ios::out).close();

            symbol_stream.open(symbol_file, std::ios::app);
            ast_stream.open(ast_file, std::ios::app);
        }
    }
    result = yyparse();
    if (__errno != 0) {
        yyerror("\nCompilation failed with %d errors.\n", INFO, -1, 0, __errno);
        return 42;
    }
    else yyerror("Compilation Succeeded.\n", INFO, -1, 1);
    return result;
}