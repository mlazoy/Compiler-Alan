#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <cassert>
#include <optional>
#include <sstream>

#include "symbol.hpp"
#include "error.hpp"

// Ensure that these are included first
#include <llvm/IR/Function.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Pass.h>

// llvm dependencies
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/Support/Casting.h>


using namespace llvm;

extern int yylineno;

class AST {
public: 
    AST() : num_line(yylineno) {}
    virtual ~AST() {}
    virtual void sem(RetType &ret_t) {}
    virtual Value* compile() = 0;
    virtual void llvm_cgen(bool opt);
    virtual void PrintTree(std::ostream &out) const = 0;

protected:  
    int num_line;

    //llvm definitions
    static LLVMContext TheContext;
    static IRBuilder<> Builder;
    static std::unique_ptr<Module> TheModule;
    static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

    static GlobalVariable *TheNL;
    static ArrayType *nl_type;

    //Library Functions
    static Function *TheWriteInteger;
    static Function *TheWriteByte;
    static Function *TheWriteChar;
    static Function *TheWriteString;

    static Function *TheReadInteger;
    static Function *TheReadByte;
    static Function *TheReadChar;
    static Function *TheReadString;

    static Function *TheShrink;
    static Function *TheExtend;
    
    static Function *TheStrLen;
    static Function *TheStrCmp;
    static Function *TheStrCpy;
    static Function *TheStrCat;

    //llvm Types
    static Type *i1;
    static Type *i8;
    static Type *i32;
    static Type *i64;
    static Type *voidTy;
    static Type *i8ptr;
    static Type *i32ptr;

    //Usefull convertions
    static ConstantInt *c1(bool b);
    static ConstantInt *c8(char c);
    static ConstantInt *c16(int n);
    static ConstantInt *c32(int n);

    //for LLVM Type conversion
    Type *llvm_type (AlanType type_t) const ;

    bool is_lib(std::string fun) const;
};

inline std::ostream & operator << (std::ostream &out, const AST &ast) {
    ast.PrintTree(out);
    return out;
}

class Stmt: virtual public AST {}; 

class Expr: virtual public AST {    //cond is also an Expr
public: 
    bool type_check(AlanType t, bool silent = false){
        // assert sem_and_compile() is called externally!!
        if (type != t) {
            if (!silent){
                std::ostringstream actual_t;
                std::ostringstream provided_t;
                actual_t << t;
                provided_t << type;
                //yyerror("Expected type '%s' but got type '%s'.\n", INFO, num_line, actual_t.str().c_str(), provided_t.str().c_str());
                yyerror("", TYPECHECKER, num_line, actual_t.str().c_str(), provided_t.str().c_str());
            }
            return false;
        }
        return true;
    }

    AlanType get_type() { return type; }


protected:
    AlanType type;
}; 

using Cond = Expr; //create alias

class Definition: public AST {};

class ExprBlock: public Expr {
public:  
    ExprBlock(): expr_list () {} 
    ~ExprBlock() {
        for (Expr *e: expr_list) delete e;
    }

    void append(Expr *e) { expr_list.push_back(e);}

    virtual void sem (RetType &ret_t) override {
        for (Expr *e: expr_list) e->sem(ret_t);    
    }

    virtual Value* compile() override {
        for (Expr *e: expr_list) e->compile();
        return nullptr;
    }

    virtual void PrintTree(std::ostream &out) const override {
        out << "ExprBlock(";
        for (auto i = expr_list.begin(); i!=expr_list.end(); ++i) {
            out << **i;
            if (i!=expr_list.end()-1) out << ", ";
        }
        out << ")";
}
private: 
    std::vector<Expr *> expr_list;  //TODO! make FunctionCall friend to access arguements

    friend class FunctionCall;
};

class Variable: public Definition {
public: 
    Variable() {} //default 
    Variable(std::string s, AlanType t): id(s), var_type(t) {}

    virtual void sem(RetType &ret_t) override {
        st.insert(id, var_type);
    }

    virtual Value* compile() override {
        RecordEntry *new_re = rt.insert(id, var_type); 
        AllocaInst *AllocaAddr = Builder.CreateAlloca(llvm_type(var_type), nullptr, id);
        new_re->allocate(AllocaAddr);
        return AllocaAddr; //or nullptr
    }

    AlanType get_type() { return var_type; }

    virtual void PrintTree(std::ostream &out) const override {
        out << "Variable(" << id << ", " << var_type << ")";
    }

protected: 
    std::string id;
    AlanType var_type;
};

class Array: public Variable {
public:  
    Array(std::string s, AlanType item_t, int l): Variable(s, AlanType(ARRAY, new AlanType(item_t))), len(l) {}

    virtual void sem(RetType &ret_t) override {
        if (!(var_type.element_type && 
        (*var_type.element_type == AlanType(INT) || *var_type.element_type == AlanType(BYTE))))
            yyerror("Unsupported type of array items. Must be either 'int' or 'byte'", FATAL, num_line);
        
        st.insert(id, var_type, new ArrayData(len));   
    }

    virtual Value* compile() override {
        RecordEntry *new_re = rt.insert(id, var_type); 
        AllocaInst *AllocaAddr; 
        Type *item_llvm = llvm_type(*var_type.element_type);
        Type * arr_type = ArrayType::get(item_llvm, len);
        AllocaAddr = Builder.CreateAlloca(arr_type, nullptr, "array");
        //new_re->allocate(AllocaAddr);
        // save BaseAddr instead ?
        Value *gep =  Builder.CreateGEP(arr_type, AllocaAddr, {c32(0), c32(0)},"base_arr");
        new_re->allocate(gep);
        return nullptr;
    }

    virtual void PrintTree(std::ostream &out) const override {
    out << "Array(" << id << ", " << var_type << ", " << len << ")";
    }

private:  
    int len;
};

class ParamBlock; //forward declaration

class Parameter: public Expr {
public:  
    Parameter() {} //default
    Parameter(char *n, bool is_ref, AlanType t): id(n), reference(is_ref), param_type(t) {}

        // they are not used
        virtual void sem(RetType &ret_t) override {}
        virtual Value *compile() override {  return nullptr; }

        void sem(SymbolEntry *parent_func) {
        FunctionData *func_data = (FunctionData *)(parent_func->get_priv());
        int idx = func_data->append_arg_t(param_type, reference);
        PrivateData *p = new ParamData(reference, idx, parent_func);
        st.insert(id, AlanType(PARAMETER), p);
        //std::cerr << "Inserted param " << id << " with type " << param_type << "\n";
        }

    void compile(RecordEntry *p_fun, int pos) { 
        ParamFrame *pf = new ParamFrame(pos, p_fun, reference);
        RecordEntry *re = rt.insert(id, AlanType(PARAMETER), pf); 

        if (!reference) {
            Value *AllocaAddr = Builder.CreateAlloca(llvm_type(param_type), nullptr, id+"_val");
            re->allocate(AllocaAddr);
        }
     }

    AlanType get_type() { return param_type; }
    std::string get_name() {return id; }
    bool is_ref() { return reference; }

    virtual void PrintTree(std::ostream &out) const override {
        out << "Parameter(" << id << ", ";
        if(reference) out << "reference, ";
        out << param_type << ")";
    }

protected:  
    std::string id;
    bool reference;
    AlanType param_type;

    friend class ParamBlock;
};

class VarBlock: public Variable {
public:  
    VarBlock(): var_list() {}
    ~VarBlock(){
        for (Variable *v: var_list) delete v;
    }

    void append(Variable *p) {var_list.push_back(p);}


    virtual void sem(RetType &ret_t) override {
        for (Variable *v: var_list) v->sem(ret_t);
    }

    virtual Value* compile() override {
        for (Variable *v: var_list) v->compile();
        return nullptr;
    }

    virtual void PrintTree(std::ostream &out) const override {
        for (auto i = var_list.begin(); i!=var_list.end(); ++i){
            out << **i;
            if (i!=var_list.end()-1) out << ", ";
        }
    }
private:  
    std::vector<Variable *> var_list;
};

class ParamBlock: public Parameter {
public:  
    ParamBlock(): param_list() {}
    ~ParamBlock(){
        for (Parameter *p: param_list) delete p;
    }

    void append(Parameter *p) {param_list.push_back(p);}

    void set_func_id(std::string f) {func_id = f;}

    virtual void sem(RetType &ret_t) override {
        SymbolEntry *E = st.lookup(func_id);
        if (E == nullptr || E->get_type().base_type != FUNCTION) 
            yyerror ("Undeclared function '%s' inside this scope.\n", FATAL, num_line,  func_id.c_str());
    
        for (Parameter *p: param_list) p->sem(E);
         
    }

    virtual Value* compile() override {
        RecordEntry *R = rt.lookup(func_id);
        //if (R == nullptr) std::cerr << "Function not found in record table" << func_id << "\n";
        //else std::cerr <<"Foun func in record table " << func_id << "\n";
        int p_idx = 0;
        for (Parameter *p: param_list) {
            p->compile(R, p_idx);
            p_idx++;
        }
        return nullptr;
    }

    void extract_llvm_types(std::vector<Type *>&vec_types) {
        for (int i = 0; i < param_list.size(); i++) {
            if (!param_list[i]->reference)
                vec_types[i] = llvm_type(param_list[i]->param_type);
            else 
                vec_types[i] = llvm_type(param_list[i]->param_type)->getPointerTo();
        }
    }

    virtual void PrintTree(std::ostream &out) const override {
        for (auto i = param_list.begin(); i!=param_list.end(); ++i){
            out << **i;
            if (i!=param_list.end()-1) out << ", ";
        }
    }

    friend class FunctionDecl; 

private:  
    std::vector<Parameter *> param_list;
    std::string func_id;
};

class IntConst: public Expr {
public:  
    IntConst(int n): value(n) {}

    virtual void sem(RetType &ret_t) override {
         type = AlanType(INT);
    }

    virtual Value* compile() override {
        return c32(value);
    }

    void PrintTree(std::ostream &out) const override {
        out << "IntConst(" << value << ")";
    }

private:  
    int value;
};

class ByteConst: public Expr {
public:  
    ByteConst(uint8_t c): ascii_val(c) {}

    virtual void sem(RetType &ret_t) override {
        type = AlanType(BYTE);
    }

    virtual Value* compile() override {
        return c8(ascii_val);
    }

    void PrintTree(std::ostream &out) const override {
        out << "ByteConst(" << ascii_val << ")";
    }

private: 
    int ascii_val;
};

class LVal: public Expr {
public: 
    AlanType get_type() { return type; }
    void set_handside(bool left) { lhandside = left; }

protected:
    bool lhandside;
};

class Id: public LVal{
public: 
    Id(char *i) : id(i) {}

    virtual void sem(RetType &ret_t) override {
        SymbolEntry *E = st.lookup(id);
        if (E != nullptr) 
            type = E->get_type();
        else 
            yyerror("Undeclared variable '%s' inside this scope.\n", FATAL, num_line, id);
    }

    virtual Value* compile() override {
        RecordEntry *R = rt.lookup(id);
        Value *AllocaAddr = R->get_alloca();

        if (lhandside) 
            return AllocaAddr;
        else 
            return Builder.CreateLoad(llvm_type(type), AllocaAddr, "id_val");
    }

    void PrintTree(std::ostream &out) const override {
        out << "Id(" << id << ")";
    }

private: 
    char *id;
};

class StringConst: public LVal {
public: 
    StringConst(char *str) {
        char c;
        uint8_t ascii_val;
        str++; //remove " from the beginning
        do {
            c = *str++;
            if (c == '\\') {
                switch(*str++){
                    case 'n'  : ascii_val = uint8_t('\n'); break;
                    case 't'  : ascii_val = uint8_t('\t'); break;
                    case 'r'  : ascii_val = uint8_t('\r'); break;
                    case '\\' : ascii_val = uint8_t('\\'); break;
                    case '\'' : ascii_val = uint8_t('\''); break;
                    case '\"' : ascii_val = uint8_t('\"'); break;
                    default   : ascii_val = uint8_t('\\'); str--; // normal charachter shouldn't be skipped
                }
            }
            else ascii_val = uint8_t(c);
            val.push_back(ascii_val);
        } while(c != '\0'); // end of string
        
        val.pop_back(); // removes '\0'
        val.pop_back(); // removes '"'
    } 

    virtual void sem(RetType &ret_t) override {
        type = AlanType(ARRAY, new AlanType(BYTE));
    }

    virtual Value* compile() override {
        Type* byteType = llvm::Type::getInt8Ty(AST::TheContext);

        std::vector<llvm::Constant*> byteValues;
        for (int i = 0; i < val.size(); i++) {
            byteValues.push_back(llvm::ConstantInt::get(byteType, val[i]));
        }
        byteValues.push_back(llvm::ConstantInt::get(byteType, 0));  // Null terminator

        ArrayType* array_type = ArrayType::get(byteType, val.size()+1); 
        llvm::Constant* constArray = llvm::ConstantArray::get(array_type, byteValues);

        GlobalVariable *globalStr = new GlobalVariable(
            *TheModule, array_type, true, GlobalValue::PrivateLinkage, constArray, "string_val");
        globalStr->setAlignment(MaybeAlign(1));

        Value* gep = Builder.CreateGEP(array_type, globalStr, {c32(0), c32(0)}, "str_ptr");
        return gep;
    }

    void PrintTree(std::ostream &out) const override {
    out << "StringConst(";
    for (int i = 0; i < val.size(); ++i) out << char(val[i]);
    out << ")";
    }

private: 
   std::vector<uint8_t> val;
};

class ArrayItem: public LVal {
public:  
    ArrayItem(char *a, Expr* i): array_id(a), index(i) {}
    ~ArrayItem() {delete index;}

    virtual void sem(RetType &ret_t) override {
        SymbolEntry *E = st.lookup(array_id);
        
        if (E == nullptr) 
            yyerror("Undeclared array '%s' in this scope.\n", FATAL, num_line, array_id);
 
        if (E->get_type().base_type == ARRAY) {
            type = (*E->get_type().element_type);
            index->sem(ret_t);
            if(!index->type_check(AlanType(INT)))
                yyerror("Invalid index expression; must be of type 'int'.\n", FATAL, num_line);
        }

        else {
            std::ostringstream t;
            t << E->get_type();
            yyerror("'%s' has type '%s' and not 'array'.\n", FATAL, num_line, array_id, t.str().c_str());
        }
    }

    virtual Value* compile () override {
        Value *pos = index->compile();
        RecordEntry *R = rt.lookup(array_id);

        Type *item_llvm = llvm_type(type);
        // check for boundaries ?
        Value *base_gep = R->get_alloca();
        assert(pos != nullptr && base_gep != nullptr);
        Value *item_gep = Builder.CreateGEP(item_llvm, base_gep, pos, "item_ptr");

        if (lhandside) return item_gep;
        else return Builder.CreateLoad(item_llvm, item_gep, "val_item");

    }

    void PrintTree(std::ostream &out) const override {
        out << "ArrayItem(" << array_id << "[" << *index << "])";
    }

private:  
    char *array_id;
    Expr *index;
};

class BoolConst: public Cond {
public:
    BoolConst(bool b): flag(b) {}

    virtual void sem(RetType &ret_t) override {
        type = AlanType(BOOL);
    }

    virtual Value* compile() override {
        if (flag) return c1(1);
        else return c1(0);
    }

    void PrintTree(std::ostream &out) const override {
        out << "BoolConst(" << flag << ")";
    }
private:  
    bool flag;
};

class Assign: public Stmt {
public: 
    Assign(LVal *lhs, Expr *rhs): lv(lhs), expr(rhs) {} //constructor
    ~Assign(){
        delete lv;
        delete expr;
    }

    virtual void sem(RetType &ret_t) override {
        expr->sem(ret_t);
        if(expr->type_check(AlanType(PROC), true))
            yyerror("RHS is of type 'proc' and can't be assigned to any variable.\n", FATAL, num_line);
        
        lv->sem(ret_t);
        if(!expr->type_check(lv->get_type())) 
            yyerror("Invalid assignment. Both sides must be of the same type.\n", FATAL, num_line);

    }

    virtual Value* compile () override {
        lv->set_handside(1);
        Value *ev = expr->compile();
        Value *AllocaAddr = lv->compile();
        lv->set_handside(0);
        assert(AllocaAddr!=nullptr && ev!= nullptr);
        return Builder.CreateStore(ev, AllocaAddr);
    }

    virtual void PrintTree(std::ostream &out) const override {
        out << "Assign(" << *lv << " = " << *expr << ")";
    }

private: 
    LVal *lv;
    Expr *expr;
};

class Return: public Stmt {
public:
    Return (Expr *e = NULL): ret(e) {} 
    ~Return(){
        delete ret;
    }

    void set_in_func(std::string func_name) { in_func = func_name; }

    virtual void sem(RetType &ret_t) override {

        if (ret){
            ret->sem(ret_t);
            type = ret->get_type();
            if (ret->type_check(AlanType(INT), true))
                ret_t = RINT;
            else if (ret->type_check(AlanType(BYTE), true)) 
                ret_t = RBYTE;
            else {
                yyerror("Invalid return statement; must be either 'int' or 'byte'.\n", FATAL, num_line);
                ret_t = UNDEF;
            }
        }

        else {
            type = AlanType(PROC);
            ret_t = VOID;
        }
    }

    virtual Value* compile() override {
        if (ret){ 
            Value *result = ret->compile();
            return Builder.CreateRet(result);
        }
        Function *currentFunction = Builder.GetInsertBlock()->getParent();
        if (currentFunction && currentFunction->getName() == "main")
            return Builder.CreateRet(c32(0));
        else 
            return Builder.CreateRetVoid();
    }

    virtual void PrintTree(std::ostream &out) const override {
        out << "return ";
        if (ret != NULL) out << "(" << *ret << ")";
    }

private:  
    Expr *ret;
    AlanType type;
    std::string in_func;
};

class While: public Stmt {
public: 
    While (Cond *c, Stmt *s): cond(c), stmt(s) {} 
    ~While(){
        delete cond;
        delete stmt;
    }

    virtual void sem(RetType &ret_t) override {
        cond->sem(ret_t);
        if (!cond->type_check(AlanType(BOOL)))
            yyerror("Invalid type expression in While condition; must be of type 'bool'.\n", FATAL, num_line);
        
        stmt->sem(ret_t);
    }

    virtual Value* compile() override {
        Value *cond_init = cond->compile();

        BasicBlock *PrevBB = Builder.GetInsertBlock();
        Function *TheFunction = PrevBB->getParent();
        BasicBlock *LoopBB = BasicBlock::Create(TheContext, "loop", TheFunction);
        BasicBlock *WhileBody = BasicBlock::Create(TheContext, "body", TheFunction);
        BasicBlock *EndLoop = BasicBlock::Create(TheContext, "end_while", TheFunction);

        Builder.CreateBr(LoopBB);
        Builder.SetInsertPoint(LoopBB);

        PHINode *phi_iter = Builder.CreatePHI(i1, 2, "iter");
        phi_iter->addIncoming(cond_init, PrevBB);
        
        Value *cmp = Builder.CreateICmpNE(phi_iter, c1(0),"while_cond");
        Builder.CreateCondBr(cmp, WhileBody, EndLoop);

        Builder.SetInsertPoint(WhileBody);

        Value *stmt_body = stmt->compile();
        Value *cond_new = cond->compile();

        phi_iter->addIncoming(cond_new, Builder.GetInsertBlock());
        Builder.CreateBr(LoopBB);
        Builder.SetInsertPoint(EndLoop);

        return nullptr;
    }

    virtual void PrintTree(std::ostream &out) const override {
        out << "while (" << *cond << ", " << *stmt << ")";
    }

private: 
    Cond *cond;
    Stmt *stmt;
};

class If: public Stmt {
public:  
    If (Cond *c, Stmt *s1, Stmt *s2 = NULL): cond(c), stmt_then(s1), stmt_else(s2) {
        ret_then = ret_else  = false; 
    }

    ~If(){
        delete cond;
        delete stmt_then;
        delete stmt_else;
    }

    virtual void sem(RetType &ret_t) override {
        cond->sem(ret_t);
        if(!cond->type_check(AlanType(BOOL)))
            yyerror("Invalid type expression in If condition; must be of type 'bool'.\n", FATAL, num_line);

        RetType then_ret_t, else_ret_t; 
        then_ret_t = else_ret_t = RNULL;

        stmt_then->sem(then_ret_t);
        if (then_ret_t != RNULL) 
            ret_then = true;

        if (stmt_else) {
            stmt_else->sem(else_ret_t);
            if (else_ret_t != RNULL) 
                ret_else = true;
        }

        if (!stmt_else) 
            ret_t = then_ret_t;

        else {
            if (then_ret_t == else_ret_t) {
                ret_t = then_ret_t;
            }
            else if (then_ret_t == RNULL) {
                ret_t = else_ret_t;
                yyerror("Return is not explicitly declared in all paths\n", WARNING, num_line);
            } 
            else if (else_ret_t == RNULL) {
                ret_t = then_ret_t;
                yyerror("Return is not explicitly declared in all paths\n", WARNING, num_line);
            }

            else {
                ret_t = UNDEF;
                yyerror("Return value must be the same for all branches.\n", FATAL, num_line);
                exit(1);
            }
        }
    
    }


    virtual Value* compile () override {
        Value *cond_val = cond->compile();

        Value *cmp = Builder.CreateICmpNE(cond_val, c1(0), "if_cond");
        // find current block
        Function *TheFunction = Builder.GetInsertBlock()->getParent();
        // create labels
        BasicBlock *BranchThen = BasicBlock::Create(TheContext, "br_then", TheFunction);
        BasicBlock *BranchElse = BasicBlock::Create(TheContext, "br_else");

        BasicBlock *EndIF = nullptr;
        
        if (!(ret_then && ret_else))
            EndIF = BasicBlock::Create(TheContext, "end_if");

        Builder.CreateCondBr(cmp, BranchThen, BranchElse);

        Builder.SetInsertPoint(BranchThen);
        stmt_then->compile();
        if (!ret_then) 
            Builder.CreateBr(EndIF); 

        // TheFunction->getBasicBlockList().push_back(BranchElse);
        TheFunction->insert(TheFunction->end(), BranchElse);
        Builder.SetInsertPoint(BranchElse);
        if (stmt_else) 
            stmt_else->compile();
        if (!ret_else) 
            Builder.CreateBr(EndIF); 
        
        if(!Builder.GetInsertBlock()->getTerminator() && EndIF)
            Builder.CreateBr(EndIF);
        if (!(ret_then && ret_else))  { 
            TheFunction->insert(TheFunction->end(), EndIF);                         
            Builder.SetInsertPoint(EndIF);
        }
        return nullptr;

    }

    virtual void PrintTree(std::ostream &out) const override {
        out << "If (" << *cond << ", " << *stmt_then ;
        if (stmt_else != NULL) out << " else" << *stmt_else ;
        out << ")";
    }


private: 
    Cond *cond;
    Stmt *stmt_then, *stmt_else;
    bool ret_then, ret_else;
};

class StmtBlock: public Stmt {
public:  
    StmtBlock(): stmt_list() {}
    ~StmtBlock(){
        for (Stmt *s: stmt_list) delete s;
    }

    void append(Stmt *s) { stmt_list.push_back(s);}

    virtual void sem(RetType &ret_t) override {
        for (Stmt *s: stmt_list) s->sem(ret_t);
        //if (dynamic_cast<Return *>(s)) ((Return *)s)->set_in_func(in_func);
    }

    virtual Value* compile() override {
        for (Stmt *s: stmt_list) s->compile();
        return nullptr;
    }

    void set_in_func(std::string func_name) { in_func = func_name; }

    virtual void PrintTree(std::ostream &out) const override {
        out << "StmtBlock(";
        for (auto i = stmt_list.begin(); i!=stmt_list.end(); ++i) {
            out << **i;
            if (i!=stmt_list.end()-1) out << ", ";
        }
        out << ")";
    }

private: 
    std::vector<Stmt *> stmt_list;
    std::string in_func;
};

class FunctionDecl; //forward declaration 

struct ScopeDef {
    ScopeDef(): var_defs(), func_defs() {}
    ~ScopeDef(){
        for (Variable *v: var_defs) delete v;
        for (FunctionDecl *fd: func_defs) delete fd;
    }

    std::vector<Variable *> var_defs;
    std::vector<FunctionDecl *> func_defs;
};

class LocalDefList: public AST {
public:  
    LocalDefList(): locals() {}

    const ScopeDef* get_locals() const {
        return &locals;
    }

    void extract_llvm_types (std::vector<Type *>&vec_types){
        for (auto &var: locals.var_defs)
            vec_types.push_back(llvm_type(var->get_type()));
    }

    void append_local_def(Definition *def);

    virtual void sem(RetType &ret_t) override;
    virtual Value* compile() override; 

    void PrintTree(std::ostream &out) const override ; 

private:  
    ScopeDef locals;
};

class FunctionDecl: public Definition {
public:   
    FunctionDecl(std::string s, ParamBlock *pb, AlanType t, StmtBlock *sb, LocalDefList *ld) : 
    id(s),ret_type(t), params(pb), body(sb), locals(ld) { 
        params->set_func_id(s); 
    }

    ~FunctionDecl(){
        delete params;
        delete body;
        delete locals;
    }

    void append_param(Parameter *p) {params->append(p);}

    virtual void sem(RetType &ret_t) override {
        ret_t = RNULL; // start with none and check final value
        FunctionData *func_priv = new FunctionData(ret_type);
        SymbolEntry *new_e = st.insert(id, AlanType(FUNCTION), func_priv);
        st.OpenScope(id);

        if(params) params->sem(ret_t);
        if (locals) locals->sem(ret_t);
        if(body) {
            body->set_in_func(id);
            body->sem(ret_t);
        }

        std::ostringstream ret_err;
        ret_err << ret_type;

        switch(ret_t) {
            case UNDEF  : yyerror("Return value diverges between paths inside '%s'; ensure is compatible with '%s'\n", FATAL, num_line, id.c_str(), ret_err.str().c_str());
                         break;
            case RINT    : if (!(ret_type == AlanType(INT)) )
                            yyerror("Return type 'int' does not match with function's '%s' return value '%s'\n", FATAL, num_line, id.c_str(), ret_err.str().c_str());
                          break;
            case RBYTE   : if (!(ret_type == AlanType(BYTE)) )
                            yyerror("Return type 'byte' does not match with function's '%s' return value '%s'\n", FATAL, num_line, id.c_str(), ret_err.str().c_str());
                          break;
            case RNULL   :
            case VOID   : if (!(ret_type == AlanType(PROC)) )
                            yyerror("End of non-void function; '%s' must return a value of '%s'\n", FATAL, num_line, id.c_str(), ret_err.str().c_str());
                          break;
            default     : yyerror("Undefined return type\n", FATAL, -1);
        }
    
        ret_t = RNULL;   // nested function should not affect parent

        // get closure
        for (auto &g: func_priv->get_closure()) {
            //std::cerr << g.first <<g.second <<std::endl;
            // if (g.first != id)
            //     closure[g.first] = g.second;
            Parameter *clo_param = new Parameter(const_cast<char *>(g.first.c_str()), true, g.second);
            params->append(clo_param);
        }

        st.closeScope();
    }

    virtual Value* compile() override {

        //if (rt.main_entry()) id = "main";
        bool first_entry = rt.main_entry();
        // int clo_sz = closure.size();
        // int sz = params ? params->param_list.size() + clo_sz : clo_sz;
        int sz = params ? params->param_list.size() : 0;
        RecordEntry *fun_rec = rt.insert(id, AlanType(FUNCTION), new FuncFrame(sz));
        FuncFrame *fun_info = (FuncFrame *)fun_rec->get_info();

        //insert globals as parameters
        // for (auto &c: closure) {
        //     RecordEntry *r_clo = rt.lookup(c.first);
        //     assert(r_clo != nullptr);
        //     Parameter *clo_p = new Parameter(const_cast<char*>(c.first.c_str()), true, c.second); //lol
        //     params->append(clo_p);
        //     fun_info->append_clo(r_clo);
        // }

        rt.openRecord(id);

        if (first_entry){  //AST has created this entry so jump immediately there
            if (locals) locals->compile();
            if (body) body->compile();
            rt.closeRecord();
            return nullptr;
        }

        BasicBlock* ParentBB = Builder.GetInsertBlock();
        
        FunctionType *FuncType;
        std::vector<Type *>frame_types;

        if(params) {
            frame_types.resize((params->param_list.size())); // holds only parameters so far
            params->extract_llvm_types(frame_types);
        }

        // insert globals as parameters
        // for (auto g: closure){
        //     frame_types.push_back(llvm_type(g.second));
        //     RecordEntry *gr = rt.lookup(g.first);
        //     fun_rec->insert_closure(gr);
        // }
        
        if(params)
            FuncType = FunctionType::get(llvm_type(ret_type), frame_types, false);
        else 
            FuncType = FunctionType::get(llvm_type(ret_type), nullptr, false);

        Function *NewFuncDef = Function::Create(FuncType, Function::InternalLinkage, id.c_str(), TheModule.get());
        BasicBlock *FuncEntry = BasicBlock::Create(TheContext, "f_entry", NewFuncDef);

        Builder.SetInsertPoint(FuncEntry);

        //set parameters names
        if (params) {
            params->compile();
            int p_idx = 0; 
            Parameter *p;
            for (Value &Arg: NewFuncDef->args()) {
                p = params->param_list.at(p_idx);
                Arg.setName(p->get_name());
                // AllocaInst *AllocaAddr = Builder.CreateAlloca(Arg.getType(), nullptr, "param_"+p_idx);
                // fun_info->stack_allocate(AllocaAddr, p_idx++);
                if(p->is_ref()) {// pass by reference
                    //std::cerr << "Passing by reference...\n";
                    fun_info->stack_allocate(&Arg, p_idx++, true);
                }
                else { // pass by value
                    Value *AllocaAddr = fun_info->get_palloca(p_idx++);
                    assert(AllocaAddr != nullptr);
                    Builder.CreateStore(&Arg, AllocaAddr);
                }
            }
        }

        if (locals)
            locals->compile();
        
        if (body) {
            body->set_in_func(id);
            body->compile();
        }
        

        if (ret_type == AlanType(PROC)) 
            Builder.CreateRetVoid(); 
            //Builder.CreateRet(c32(0));
        
        // else {
        //     AllocaInst *AllocaAddr = fun_info->get_res_alloca();
        //     Value *ret = Builder.CreateLoad(llvm_type(ret_type), AllocaAddr);
        //     Builder.CreateRet(ret); 
        // }

        Builder.SetInsertPoint(ParentBB); // Return to previous builder block.
    
        rt.closeRecord();
        return nullptr;
    }

    void PrintTree(std::ostream &out) const override {
        out << "FuncDecl(" << id << ", [" << *params << "], " << ret_type << ", LocalDefs{\n" ;
        out << *locals << "}, ";
        out << "Body{\n" << *body << "\n})" ;
    }

private:  
    std::string id;
    AlanType ret_type;
    ParamBlock *params;
    StmtBlock *body;
    LocalDefList *locals;
    //std::map<std::string, AlanType> closure;
};

inline void LocalDefList::append_local_def(Definition *def) {
        if (auto var = dynamic_cast<Variable *>(def)) 
            locals.var_defs.push_back(var);
        else if (auto func = dynamic_cast<FunctionDecl *>(def))
            locals.func_defs.push_back(func);
        else {
            std::cerr << "Error: Unsupported type to append" << std::endl;
        }
}

inline void LocalDefList::PrintTree(std::ostream &out) const {
        for (auto i=locals.var_defs.begin(); i!=locals.var_defs.end(); ++i){
            out << **i;
            if (i!=locals.var_defs.end()-1) out << ", ";
        } 
        if (locals.func_defs.begin()!=locals.func_defs.end()) out << ", ";
        for (auto j=locals.func_defs.begin(); j!=locals.func_defs.end(); ++j){
            out << **j;
            if (j!=locals.func_defs.end()-1) out << ", ";
        }
    }

inline void LocalDefList::sem(RetType &ret_t) {
    for (Variable *v: locals.var_defs) v->sem(ret_t);
    for (FunctionDecl *fd: locals.func_defs) fd->sem(ret_t);
}

inline Value* LocalDefList::compile() {
    for (Variable *v: locals.var_defs) v->compile();
    for (FunctionDecl *fd: locals.func_defs) fd->compile();
    return nullptr;
}

class FunctionCall: public Stmt, public Expr {
public: 
    FunctionCall(std::string s, ExprBlock *a = nullptr): id(s), args(a) {}
    ~FunctionCall(){
        delete args;
    }
        
    virtual void sem(RetType &ret_t) override {
        SymbolEntry *E = st.lookup(id);
        if (E == nullptr) 
            yyerror("Undeclared function '%s' inside this scope.\n", FATAL, num_line, id.c_str());

        if (E->get_type().base_type != FUNCTION) 
            yyerror("'%s' has type '%s' and is not a function.\n", FATAL, num_line, id.c_str(), E->get_type().base_type);

        FunctionData *func_data = (FunctionData *)E->get_priv();

        //set type if used as Expr
        type = func_data->get_ret_type();

        //check if len(args) == len(params) && arg[i] matches parameter[i]
        int arg_size = (args == nullptr) ? 0 : args->expr_list.size();
        if (arg_size != func_data->get_arg_num()) 
            yyerror("Calling '%s' requires %d arguements but %d provided.\n", FATAL, num_line, id.c_str(), func_data->get_arg_num(), arg_size);

        if (args) {
            int i = 0;
            for (Expr *a: args->expr_list){
                a->sem(ret_t);
                AlanType p = func_data->get_param_type(i++);
                if(!a->type_check(p))
                    yyerror("Calling '%s' with imcompatible arguement type at position %d.\n", FATAL, num_line, id.c_str(), i-1);
            }
        }
        // push closure arguements
        for (auto &clo_arg: func_data->get_closure()){
            Expr *clo_expr = new Id(const_cast<char *>(clo_arg.first.c_str()));
            args->append(clo_expr);
        }
    }

    virtual Value* compile() override {
        Function *callee = TheModule.get()->getFunction(id.c_str());
        if (!callee) 
            yyerror("Callee Function not found\n", FATAL, num_line);

        int args_size = (args == nullptr) ? 0 : args->expr_list.size();
        std::vector<Value *>arg_vals(args_size);

        RecordEntry *re = rt.lookup(id);
        FuncFrame *fun_info = (FuncFrame *)re->get_info();

        if (args) {
            int i = 0;
            Value *a_val;
            for (Expr *a: args->expr_list){
                if (fun_info->get_ref(i)){
                    dynamic_cast<LVal *>(a)->set_handside(1);
                    a_val = a->compile();
                    dynamic_cast<LVal *>(a)->set_handside(0);
                    //assert (llvm::isa<llvm::AllocaInst>(a_val));
                }
                else 
                    a_val = a->compile();
                arg_vals[i++] = a_val;
            }
        }

        //pass global variables from closure by reference
        // for (auto r_clo: fun_info->get_clo()){
        //     Value *AllocaAddr = r_clo->get_alloca();
        //     arg_vals.push_back(AllocaAddr);
        // }

        CallInst *caller;
        if (type != PROC)
            caller = Builder.CreateCall(callee, arg_vals, "func_jmp"); 
        else 
            caller = Builder.CreateCall(callee, arg_vals);

        //result
        if (callee->getReturnType() != Type::getVoidTy(TheContext)) 
            return caller;
             
        return nullptr;
    }

    void PrintTree(std::ostream &out) const override {
        out << "FuncCall(" << id << ", [";
        if (args != nullptr) out << *args;
        out << "])";
    }

private:  
    std::string id;
    ExprBlock *args;
};

class BinaryExpr: public Expr {
public:  
    BinaryExpr(char o, Expr *l, Expr *r): op(o), left_op(l), right_op(r) {}
    ~BinaryExpr(){
        delete left_op;
        delete right_op;
    }

    virtual void sem(RetType &ret_t) override {
        AlanType temp_l, temp_r;
        left_op->sem(ret_t);
        switch(op){
            // only booleans
            case '|': case '&' : 
            if(!left_op->type_check(AlanType(BOOL))) {
                yyerror("Invalid left operand type in '%c' expression. Must be 'bool'.\n", FATAL, num_line, op);
                break;
            }
            right_op->sem(ret_t);
            if(!right_op->type_check(AlanType(BOOL))) {
                yyerror("Invalid right operand type in '%c' expression. Must be 'bool'.\n", FATAL, num_line, op);
                break;
            }
            type = AlanType(BOOL);
            break;

            // ints or bytes
            case '-' : case '+' : case '*' : case '/' : case '%' : 
            case '>' : case '<' : case 'g' : case 'l' : 
            if(left_op->type_check(AlanType(INT), true)) temp_l = AlanType(INT);
            else if (left_op->type_check(AlanType(BYTE), true)) temp_l = AlanType(BYTE);
            else {
                yyerror("Invalid left operand type in '%c' expression. Must be either 'int' or 'byte'.\n", FATAL, num_line, op);
                break;
            }
            right_op->sem(ret_t);
            if(!right_op->type_check(temp_l)) {
                yyerror("Both opernads must be of the same type\n", FATAL, num_line);
                break;
            }
            switch(op) {
                case 'g' : case 'l' : case '>' : case '<' :
                type = AlanType(BOOL); break;

                default: type = temp_l; 
            }
            break;
            //int, bytes, booleans
            case 'e' : case 'n' : 
            if(left_op->type_check(AlanType(INT), true)) temp_l = AlanType(INT);
            else if (left_op->type_check(AlanType(BYTE), true)) temp_l = AlanType(BYTE);
            else if (left_op->type_check(AlanType(BOOL), true)) temp_l = AlanType(BOOL);
            else {
                yyerror("Invalid left operand type in '%c' expression. Must be 'int' or 'byte' or 'bool'.\n", FATAL, num_line, op);
                break;
            }
            right_op->sem(ret_t);
            if(!right_op->type_check(temp_l)) {
                yyerror("Both opernads must be of the same type\n", FATAL, num_line);
                break;
            }
            type = AlanType(BOOL);
        }
    }

    virtual Value* compile() override {
        Value *lv, *rv;
        lv = left_op->compile();
        //rv = right_op->compile();

        switch(op){
            case '+': rv = right_op->compile(); return Builder.CreateAdd(lv, rv, "res_add");
            case '-': rv = right_op->compile(); return Builder.CreateSub(lv, rv, "res_sub");
            case '*': rv = right_op->compile(); return Builder.CreateMul(lv, rv, "res_mul");
            case '/': rv = right_op->compile(); return Builder.CreateSDiv(lv, rv, "res_div");
            case '%': rv = right_op->compile(); return Builder.CreateSRem(lv, rv, "res_mod");

            case '>': rv = right_op->compile(); return Builder.CreateICmpSGT(lv, rv, "res_g"); //all comparisons are signed!
            case '<': rv = right_op->compile(); return Builder.CreateICmpSLT(lv, rv, "res_l");
            case 'g': rv = right_op->compile(); return Builder.CreateICmpSGE(lv, rv, "res_ge");
            case 'l': rv = right_op->compile(); return Builder.CreateICmpSLE(lv, rv, "res_le");
            case 'e': rv = right_op->compile(); return Builder.CreateICmpEQ(lv, rv, "res_eq");
            case 'n': rv = right_op->compile(); return Builder.CreateICmpNE(lv, rv, "res_nq");
            // | and & become short circuit
            case '|': { //return Builder.CreateLogicalOr(lv, rv, "res_or");
                Function *currFunc = Builder.GetInsertBlock()->getParent();
                BasicBlock *shortCircuit = BasicBlock::Create(TheContext, "short_circuit", currFunc);
                BasicBlock *evalRight = BasicBlock::Create(TheContext, "eval_right", currFunc);
                BasicBlock *merge = BasicBlock::Create(TheContext, "merge", currFunc);

                // Check if the left operand is true
                Builder.CreateCondBr(lv, shortCircuit, evalRight);

                // Short-circuit block
                Builder.SetInsertPoint(shortCircuit);
                Value *trueValue = ConstantInt::get(Type::getInt1Ty(TheContext), 1); // true
                Builder.CreateBr(merge);

                // Evaluate right operand
                Builder.SetInsertPoint(evalRight);
                rv = right_op->compile();
                Builder.CreateBr(merge);

                // Merge block
                Builder.SetInsertPoint(merge);
                PHINode *phi = Builder.CreatePHI(Type::getInt1Ty(TheContext), 2, "res_or");
                phi->addIncoming(trueValue, shortCircuit);
                phi->addIncoming(rv, evalRight);

                return phi;
            } 
            case '&': { //return Builder.CreateLogicalAnd(lv, rv, "res_and");
                Function *currFunc = Builder.GetInsertBlock()->getParent();
                BasicBlock *evalRight = BasicBlock::Create(TheContext, "eval_right", currFunc);
                BasicBlock *shortCircuit = BasicBlock::Create(TheContext, "short_circuit", currFunc);
                BasicBlock *merge = BasicBlock::Create(TheContext, "merge", currFunc);

                // Check if the left operand is false
                Builder.CreateCondBr(lv, evalRight, shortCircuit);

                // Short-circuit block
                Builder.SetInsertPoint(shortCircuit);
                Value *falseValue = ConstantInt::get(Type::getInt1Ty(TheContext), 0); // false
                Builder.CreateBr(merge);

                // Evaluate right operand
                Builder.SetInsertPoint(evalRight);
                rv = right_op->compile();
                Builder.CreateBr(merge);

                // Merge block
                Builder.SetInsertPoint(merge);
                PHINode *phi = Builder.CreatePHI(Type::getInt1Ty(TheContext), 2, "res_and");
                phi->addIncoming(falseValue, shortCircuit);
                phi->addIncoming(rv, evalRight);

                return phi;
            }
        }
        // this point is never reached!
        return nullptr;
    }

    void PrintTree(std::ostream &out) const override {
        out << "BinOp(" << op << ", " << *left_op << ", " << *right_op << ")";
    }

private:  
    Expr *left_op, *right_op;
    char op;
};

class UnaryExpr: public Expr {
public:  
    UnaryExpr(char o, Expr *e): op(o), expr(e) {} 
    ~UnaryExpr(){
        delete expr;
    }

    virtual void sem(RetType &ret_t) override {
        expr->sem(ret_t);
        switch(op){
            case '+': case '-': 
                if(expr->type_check(AlanType(INT))) 
                    type = AlanType(INT);
                else 
                    yyerror("Invalid type expression\n", FATAL, num_line); 
                break;
            
            case '!': 
                if(expr->type_check(AlanType(BOOL))) 
                    type = AlanType(BOOL);
                else 
                    yyerror("Invalid type expression\n", FATAL, num_line);

            default: break;
        }
    }

    virtual Value* compile() override {
        Value *ev = expr->compile();
        switch(op){
            case '+': return ev;
            case '-': return Builder.CreateNeg(ev,"neg");
            case '!': return Builder.CreateNot(ev, "not");
             // this point is never reached !
            return nullptr;
        }
    }

    void PrintTree(std::ostream &out) const override {
        out << "UnOp(" << op << ", " << *expr << ")";      //assuming op is single charachter
    }

private:  
    char op; 
    Expr *expr;
};

#endif