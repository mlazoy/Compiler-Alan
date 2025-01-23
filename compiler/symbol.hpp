#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include <iostream>
#include <map>
#include <unordered_set>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <sstream>
#include <fstream>

#include "error.hpp"

extern bool DEBUG;
extern std::ofstream symbol_stream;

enum BaseType {NONE, INT, BYTE, BOOL, ARRAY, PROC, FUNCTION, PARAMETER};

enum RetType {UNDEF, VOID, RINT, RBYTE, RNULL};

struct AlanType {
    BaseType base_type;
    AlanType *element_type;

    AlanType() {}   //default constructor
    AlanType(BaseType t): base_type(t), element_type(nullptr) {}
    AlanType(BaseType base_t, AlanType *elem_t): base_type(base_t), element_type(elem_t) {}

    bool operator == (const AlanType &other) const {
        if(base_type != other.base_type) 
            return false;
        else if (element_type == nullptr && other.element_type == nullptr)
            return true;
        else if (element_type != nullptr && other.element_type != nullptr)
            return *element_type == *other.element_type; //recursively compare type-constructors
        
        return false;
    }

    bool operator != (const AlanType &other) const {
        return ! (*this == other);
    }
};

inline std::ostream& operator << (std::ostream &out, const AlanType &t) {
    switch(t.base_type) {
        case NONE: out << "_?"; break;
        case INT: out << "int"; break; 
        case BYTE: out << "byte"; break;
        case BOOL: out << "bool"; break;
        case ARRAY: out << "array of " << *t.element_type; break;
        case PROC: out << "proc"; break;
        case FUNCTION: out <<"function"; break;
        case PARAMETER: out <<"parameter"; break;
        default: out << "Unsupported data-type\n";
        }    
    return out;
    }

struct SymbolEntry; //forward declaration


class PrivateData{
public:  
    virtual void PrintData(std::ostream& os) const = 0;
};

class ArrayData: public PrivateData {
public:   
    ArrayData(int l): length(l) {}
    ~ArrayData() {}

    virtual void PrintData(std::ostream& os) const override {
        os << "ArrayData(length: " << length << ")";
    }

    int get_length() { return length; }

private:
    int length;
};


class FunctionData: public PrivateData {
public:  
    FunctionData(AlanType r): ret_type(r), args(), closure () {}

    ~FunctionData() {}

    virtual void PrintData(std::ostream& os) const override {
        os << "FunctionData(return type: " << ret_type << ", params: " << args.size() << " {";
        for (const Param& p: args) 
            os << p.p_type << ", ";
        if (closure.size() > 0){
        os << "}, clo: {";
        for (auto var: closure)
            os << var.first << ", ";
        os << "}";
        }
        os << ")";
    }

    int append_arg_t(AlanType t, bool ref){
        args.push_back(Param(t,ref));
        return args.size()-1;
    }

    AlanType get_ret_type() { return ret_type; }

    AlanType get_param_type(int idx){
        if (idx >= args.size()) {
            std::cerr << "Index out of range\n";
            exit(1);
        }
        return args[idx].p_type;
    }

    bool get_reference(int idx) {
        if (idx >= args.size()) {
            std::cerr << "Index out of range\n";
            exit(1);
        }
        return args[idx].is_ref;
    }


    int get_arg_num() { return args.size(); }

    void add_closure(std::string s, AlanType t) { closure[s] = t; }
    const std::map<std::string,AlanType>& get_closure() const { return closure; }
    int get_clo_size() { return closure.size(); }


private: 
    AlanType ret_type;

    struct Param{
        Param() {} 
        Param(AlanType t, bool r): p_type(t), is_ref(r) {}
        AlanType p_type;
        bool is_ref;
    };
    std::vector<Param> args;
    std::map<std::string, AlanType> closure;
};

class ParamData: public PrivateData {
public: 
    ParamData(bool ref, int p, SymbolEntry *f) : reference(ref), pos(p), parent_func(f) {}
    ~ParamData() {}

    virtual void PrintData (std::ostream& os) const override {
        os << "ParamData(reference: " << reference << ", pos: " << pos << ")\n";
    }

    bool get_reference() { return reference; }
    int get_pos() { return pos; }
    SymbolEntry *get_parent() { return parent_func; }

private:
    bool reference;
    int pos;
    SymbolEntry *parent_func;
};
 

struct SymbolEntry {
public:
    SymbolEntry() {}
    SymbolEntry(AlanType t): type(t), priv(nullptr), unused(true) {}
    SymbolEntry(AlanType t, PrivateData *p): type(t), priv(p), unused(true) {}

    ~SymbolEntry() {}

    AlanType get_type() { 
        if (type == PARAMETER) {
            ParamData *param_data = (ParamData *)priv;
            //std::cerr << "idx : " << idx << std::endl;
            SymbolEntry *parent_func = param_data->get_parent();
            FunctionData *parent_priv = (FunctionData *)(parent_func->get_priv());
            return parent_priv->get_param_type(param_data->get_pos());
        }
        return type; 
    }

    PrivateData *get_priv() { return priv; }

    void set_used() {unused = false;}
    bool is_unused() const {return unused;}

private:
    AlanType type;
    PrivateData *priv;
    bool unused;
};

class Scope {
public:  
    Scope(std::string s) : f_name(s) {}

    SymbolEntry* insert(std::string s, AlanType t, PrivateData *p = nullptr) {
        //check if variable is already declared
        if(locals.find(s) != locals.end()) {
            //yyerror("Redefinition error: '%s' already exists in this scope\n", FATAL, -1, s.c_str());
            return nullptr; // trigger error from caller in ast.hpp
        }

        locals[s] = SymbolEntry(t,p);
        return &locals[s];
    }

    SymbolEntry *lookup (std::string s){
        if(locals.find(s) == locals.end()) return nullptr;
        locals[s].set_used();
        return &locals[s];
    }

    void PrintElements(std::ostream& os) {
        for (auto& i: locals){
            os << i.first << ": " << i.second.get_type();
            if (i.second.get_priv()){
                os <<" -> ";
                i.second.get_priv()->PrintData(os);
            }
            os << "\n" ;
        }
    }

    void check_unused_variables() {
        for (const auto& entry : locals) {
            if (entry.second.is_unused()) 
                yyerror("Variable '%s' is unused\n", WARNING, -1, entry.first.c_str());
        }
    }

private: 
    std::map<std::string, SymbolEntry> locals;
    std::string f_name;

    friend class SymbolTable;
};

class SymbolTable {
public:     

    SymbolEntry *lookup_scope(std::string s){
        SymbolEntry *e = scopes.rbegin()->lookup(s);
        return e;
    }

    SymbolEntry *lookup_lib(std::string s){
        SymbolEntry *e = (scopes.begin())->lookup(s);
        return e;
    }

    SymbolEntry *lookup_global(std::string s){
        for (auto i=scopes.rbegin(); i != scopes.rend(); i++){
            SymbolEntry *e = i->lookup(s);
            if (e != nullptr) { // add to closure of current func
                if (e->get_type() != AlanType(FUNCTION)) {
                    SymbolEntry *parent = lookup(scopes.back().f_name);
                    FunctionData *f_data = (FunctionData *)parent->get_priv();
                    f_data->add_closure(s,e->get_type());
                }
                return e;
            }
        }
        return nullptr;
    }

    SymbolEntry *lookup(std::string s) {
        SymbolEntry *e = lookup_scope(s);
        if (e != nullptr) return e;

        e = lookup_lib(s);
        if (e != nullptr) return e;

        e = lookup_global(s);
        return e;
    }

    SymbolEntry* insert (std::string s, AlanType t, PrivateData *p = nullptr) {
        return scopes.back().insert(s, t, p);
    }

    void OpenScope(std::string f) {
        scopes.push_back(Scope(f));
        if (DEBUG) symbol_stream << f << "'s Scope {\n\n"; 
    }

    void closeScope() {
        if (scopes.size() > 2)
            scopes.back().check_unused_variables();
        if (DEBUG) {
            scopes.back().PrintElements(symbol_stream);
            symbol_stream << "}End of " << scopes.back().f_name << "'s Scope\n\n"; 
        }
        scopes.pop_back();
    }

 void init() {
        if(DEBUG) {
            //std::cerr << "\nSYMBOL TABLE: \n\n";
            symbol_stream << "SYMBOL TABLE: \n\n";
        }
        
        OpenScope("LibAlan");
        SymbolEntry *new_entry;
        new_entry = insert("writeInteger", AlanType(FUNCTION), new FunctionData(AlanType(PROC)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(INT), false);
        new_entry = insert("writeByte", AlanType(FUNCTION), new FunctionData(AlanType(PROC)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(BYTE), false);
        new_entry = insert("writeChar", AlanType(FUNCTION), new FunctionData(AlanType(PROC)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(BYTE), false);
        new_entry = insert("writeString", AlanType(FUNCTION), new FunctionData(AlanType(PROC)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);

        new_entry = insert("readInteger", AlanType(FUNCTION), new FunctionData(AlanType(INT)));
        new_entry = insert("readByte", AlanType(FUNCTION), new FunctionData(AlanType(BYTE)));
        new_entry = insert("readChar", AlanType(FUNCTION), new FunctionData(AlanType(BYTE)));
        new_entry = insert("readString", AlanType(FUNCTION), new FunctionData(AlanType(PROC)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(INT), false);
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);

        new_entry = insert("extend", AlanType(FUNCTION), new FunctionData(AlanType(INT)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(BYTE), false);
        new_entry = insert("shrink", AlanType(FUNCTION), new FunctionData(AlanType(BYTE)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(INT), false);

        new_entry = insert("strlen", AlanType(FUNCTION), new FunctionData(AlanType(INT)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);
        new_entry = insert("strcmp", AlanType(FUNCTION), new FunctionData(AlanType(INT)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);
        new_entry = insert("strcpy", AlanType(FUNCTION), new FunctionData(AlanType(PROC)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);
        new_entry = insert("strcat", AlanType(FUNCTION), new FunctionData(AlanType(PROC)));
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);
        ((FunctionData *)new_entry->get_priv())->append_arg_t(AlanType(ARRAY, new AlanType(BYTE)), true);

        OpenScope("main");
        // main goes here
        }

        void clear () {
            closeScope(); closeScope();
        }


private:
    std::vector<Scope> scopes;
};

extern SymbolTable st;

struct RecordEntry; //fw

class StackFrame {};

class FuncFrame : public StackFrame {
public:
    FuncFrame (int size): local_stack(size), closure(), references(size, false) {}
    ~FuncFrame () {}

    void stack_allocate(llvm::Value *addr, int p, bool r) { 
        local_stack[p] = addr; 
        if (r) set_ref(p);
    }

    llvm::Value *get_palloca(int p) { return local_stack.at(p); }

    bool get_ref(int p) { return references.at(p); }
    void set_ref(int p) { references[p] = true; }

    void append_clo(RecordEntry *g) { closure.push_back(g); }
    const std::vector<RecordEntry *>& get_clo() const { return closure; }
    int get_clo_size() { return closure.size(); }


private:
    std::vector<llvm::Value *>local_stack;
    std::vector<RecordEntry *>closure;
    std::vector<bool>references;
};


class ParamFrame : public StackFrame {
public:
    ParamFrame(int i, RecordEntry *p, bool r) : idx(i), parent(p), ref(r) {}
    ~ParamFrame () {}

    llvm::Value *get_palloca(); 
    void p_allocate(llvm::Value *addr);


private:
    int idx;
    bool ref;
    RecordEntry *parent;
};

struct RecordEntry {
public:
    RecordEntry () {}
    RecordEntry(AlanType t) : type(t) {}
    RecordEntry(AlanType t, StackFrame *s) : type(t), info(s) {}
    ~RecordEntry() {}

    StackFrame *get_info() { return info; }

    virtual void allocate(llvm::Value *addr) { 
        if (type.base_type != PARAMETER)
            AllocaAddr = addr; 
        else ((ParamFrame *)info)->p_allocate(addr);
    }

    virtual llvm::Value *get_alloca() { 
        if (type.base_type != PARAMETER)
            return AllocaAddr; 
        else 
            return ((ParamFrame *)info)->get_palloca();
    }

protected:
    AlanType type;
    StackFrame *info;
    llvm::Value *AllocaAddr;
};

    inline llvm::Value *ParamFrame::get_palloca() {
        FuncFrame *p_info = (FuncFrame *)(parent->get_info());
        return p_info->get_palloca(idx);
    }

    inline void ParamFrame::p_allocate(llvm::Value *addr) {
        FuncFrame *p_info = (FuncFrame *)(parent->get_info());
        p_info->stack_allocate(addr, idx, ref);
    }


class Record {
public:
    Record(std::string f) : f_name(f) {}

    RecordEntry *insert(std::string s, AlanType t, StackFrame *st){
        locals[s] = RecordEntry(t, st);
        return &locals[s];
    }


    RecordEntry *lookup (std::string s) {
        if (locals.find(s) == locals.end()) return nullptr;
        return &locals[s];
    }

    void print_records(std::ostream& os) {
        for (auto rec: locals){
            os << rec.first << "\n";
        }
    }

private:
    std::map<std::string, RecordEntry> locals;
    std::string f_name;

    friend class RecordTable;
};

class RecordTable {
public:
    RecordTable () {}

    RecordEntry *lookup(std::string s) {
        for (auto r = records.rbegin(); r != records.rend(); r++){
            RecordEntry *re = r->lookup(s);
            if (re != nullptr) return re;
        }
        return nullptr;
    }

    bool main_entry() {
        return (records.size() == 2);
    }

    RecordEntry *insert (std::string s, AlanType t, StackFrame *st = nullptr) { 
        return records.back().insert(s,t,st); 
    }

    void openRecord(std::string f){
        if (DEBUG) 
            symbol_stream << f << "'s Activation Record { \n";
        records.push_back(Record(f));
    }

    void closeRecord (){
        if (DEBUG){
            records.back().print_records(symbol_stream);
            symbol_stream << "} End of " << records.back().f_name << "'s Record\n";
        }
        records.pop_back();
    }

    void init() {
        if (DEBUG) symbol_stream << "\nACTIVATION RECORDS: \n\n";
        RecordEntry *fun_lib;
        //FuncFrame *fun_info;
        openRecord("LibAlan");
        // lib funs go here
        fun_lib = insert("writeInteger", AlanType(FUNCTION), new FuncFrame(1));
        fun_lib = insert("writeByte", AlanType(FUNCTION), new FuncFrame(1));
        fun_lib = insert("writeChar", AlanType(FUNCTION), new FuncFrame(1));
        fun_lib = insert("writeString", AlanType(FUNCTION), new FuncFrame(1));
        ((FuncFrame *)fun_lib->get_info())->set_ref(0);

        fun_lib = insert("readInteger", AlanType(FUNCTION), new FuncFrame(0));
        fun_lib = insert("readByte",  AlanType(FUNCTION), new FuncFrame(0));
        fun_lib = insert("readChar",  AlanType(FUNCTION), new FuncFrame(0));
        fun_lib = insert("readString", AlanType(FUNCTION), new FuncFrame(2));
        ((FuncFrame *)fun_lib->get_info())->set_ref(1);

        fun_lib = insert("shrink", AlanType(FUNCTION), new FuncFrame(1));
        fun_lib = insert("extend", AlanType(FUNCTION), new FuncFrame(1));

        fun_lib = insert("strlen", AlanType(FUNCTION), new FuncFrame(1));
        ((FuncFrame *)fun_lib->get_info())->set_ref(0);
        fun_lib = insert("strcmp", AlanType(FUNCTION), new FuncFrame(2));
        ((FuncFrame *)fun_lib->get_info())->set_ref(0);
        ((FuncFrame *)fun_lib->get_info())->set_ref(1);
        fun_lib = insert("strcpy", AlanType(FUNCTION), new FuncFrame(2));
        ((FuncFrame *)fun_lib->get_info())->set_ref(0);
        ((FuncFrame *)fun_lib->get_info())->set_ref(1);
        fun_lib = insert("strcat", AlanType(FUNCTION), new FuncFrame(2));
        ((FuncFrame *)fun_lib->get_info())->set_ref(0);
        ((FuncFrame *)fun_lib->get_info())->set_ref(1);
       

        openRecord("main");
        // main goes here

    }

    void clear() {
        closeRecord(); closeRecord();
    }


private:
    std::vector<Record> records;
};

extern RecordTable rt;

#endif