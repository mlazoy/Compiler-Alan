#include "ast.hpp"

#include <set>

LLVMContext AST::TheContext;
IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<Module> AST::TheModule;
std::unique_ptr<legacy::FunctionPassManager> AST::TheFPM;

Type *AST::i1;
Type *AST::i8;
Type *AST::i32;
Type *AST::i64;
Type *AST::voidTy;
Type *AST::i8ptr;
Type *AST::i32ptr;

ArrayType *AST::nl_type;
GlobalVariable *AST::TheNL;

Function *AST::TheWriteInteger;
Function *AST::TheWriteByte;
Function *AST::TheWriteChar;
Function *AST::TheWriteString;

Function *AST::TheReadInteger;
Function *AST::TheReadByte;
Function *AST::TheReadChar;
Function *AST::TheReadString;

Function *AST::TheShrink;
Function *AST::TheExtend;

Function *AST::TheStrLen;
Function *AST::TheStrCmp;
Function *AST::TheStrCpy;
Function *AST::TheStrCat;

 void AST::llvm_cgen(bool opt) {
     TheModule = std::make_unique<Module>("alan program", TheContext);
        TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());
        if (opt){
            TheFPM->add(createPromoteMemoryToRegisterPass());
            TheFPM->add(createInstructionCombiningPass());
            TheFPM->add(createReassociatePass());
            TheFPM->add(createGVNPass());
            TheFPM->add(createCFGSimplificationPass());
        }
        TheFPM->doInitialization();

        //Initialize types
        i1 = IntegerType::get(TheContext, 1);
        i8 = IntegerType::get(TheContext, 8);
        i32 = IntegerType::get(TheContext, 32);
        i64 = IntegerType::get(TheContext, 64);
        voidTy = Type::getVoidTy(TheContext);  
        i8ptr = PointerType::get(i8, 0); 
        i32ptr = PointerType::get(i32, 0); 

        // Initialise the newline array
        ArrayType *nl_type = ArrayType::get(i8, 2); //TODO! add other special chars here!
        TheNL = new GlobalVariable(
            *TheModule, nl_type, true, GlobalValue::PrivateLinkage,
            ConstantArray::get(nl_type, {c8('\n'), c8('\0')}), "nl");
        TheNL->setAlignment(MaybeAlign(1));

        // Initialize Library Functions here
        // ---------------------------------------------------------------------------------------------------------
        // Write Functions 
        FunctionType *wr_int_type = FunctionType::get(voidTy, {i32}, false);
        TheWriteInteger = Function::Create(wr_int_type, Function::ExternalLinkage, "writeInteger", TheModule.get());

        FunctionType *wr_byte_type = FunctionType::get(voidTy, {i8}, false);
        TheWriteByte = Function::Create(wr_byte_type, Function::ExternalLinkage, "writeByte", TheModule.get());

        FunctionType *wr_char_type = FunctionType::get(voidTy, {i8}, false);
        TheWriteByte = Function::Create(wr_char_type, Function::ExternalLinkage, "writeChar", TheModule.get());

        FunctionType *wr_str_type = FunctionType::get(voidTy, {i8ptr}, false);
        TheWriteString = Function::Create(wr_str_type, Function::ExternalLinkage, "writeString", TheModule.get());

        // Read Functions
        FunctionType *rd_int_type = FunctionType::get(i32, {}, false);
        TheReadInteger = Function::Create(rd_int_type, Function::ExternalLinkage, "readInteger", TheModule.get());

        FunctionType *rd_byte_type = FunctionType::get(i8, {}, false);
        TheReadByte = Function::Create(rd_byte_type, Function::ExternalLinkage, "readByte", TheModule.get());

        FunctionType *rd_char_type = FunctionType::get(i8, {}, false);
        TheReadChar = Function::Create(rd_char_type, Function::ExternalLinkage, "readChar", TheModule.get());

        FunctionType *rd_str_type = FunctionType::get(voidTy, {i32, i8ptr}, false);
        TheReadString = Function::Create(rd_str_type, Function::ExternalLinkage, "readString", TheModule.get());

        // Conversion Functions
        FunctionType *shr_type = FunctionType::get(i8, {i32}, false);
        TheShrink = Function::Create(shr_type, Function::ExternalLinkage, "shrink", TheModule.get());

        FunctionType *ext_type = FunctionType::get(i32, {i8}, false);
        TheExtend = Function::Create(ext_type, Function::ExternalLinkage, "extend", TheModule.get());

        // String Functions
        FunctionType *str_len_type = FunctionType::get(i32, {i8ptr}, false);
        TheStrLen = Function::Create(str_len_type, Function::ExternalLinkage, "strlen", TheModule.get());

        FunctionType *str_cmp_type = FunctionType::get(i32, {i8ptr, i8ptr}, false);
        TheStrCmp = Function::Create(str_cmp_type, Function::ExternalLinkage, "strcmp", TheModule.get());

        FunctionType *str_cpy_type = FunctionType::get(voidTy, {i8ptr, i8ptr}, false);
        TheStrCpy = Function::Create(str_cpy_type, Function::ExternalLinkage, "strcpy", TheModule.get());

        FunctionType *str_cat_type = FunctionType::get(voidTy, {i8ptr, i8ptr}, false);
        TheStrCat = Function::Create(str_cat_type, Function::ExternalLinkage, "strcat", TheModule.get());
        // End Of Library Functions
        // ---------------------------------------------------------------------------------------------------------
      
        //Define the main function here
        FunctionType *main_type = FunctionType::get(i32, {}, false);
        Function *main = Function::Create(main_type, Function::ExternalLinkage, 
                                        "main", TheModule.get());
        //Initialize the entry block here
        BasicBlock *BB = BasicBlock::Create(TheContext, "entry", main);
        Builder.SetInsertPoint(BB);
        
        //Program code here
        compile();
        Builder.CreateRet(c32(0)); //end of code return value

        //Verifies the IR
        bool oups = verifyModule(*TheModule, &errs());
        if (oups){
            std::cerr << "Oups! The IR has errors." << std::endl;
            TheModule->print(errs(), nullptr);
            std::exit(1);
        }

        //Optimizations
        TheFPM->run(*main);
        //Print out the IR. Later save this in a file
        TheModule->print(outs(), nullptr);
 }

    ConstantInt* AST::c1 (bool b){
        return ConstantInt::get(TheContext, APInt(1, b, true));
    }

    ConstantInt* AST::c8 (char c){
        return ConstantInt::get(TheContext, APInt(8, c, true));
    }

    ConstantInt* AST::c16 (int n) {
        return ConstantInt::get(TheContext, APInt(16, n, true));
    }

    ConstantInt* AST::c32 (int n){
        return ConstantInt::get(TheContext, APInt(32, n ,true));
    }

    // ALAN->LLVM Type conversion
    Type* AST::llvm_type (AlanType type_t) const {
        switch(type_t.base_type){
            case PROC : return voidTy;
            case BYTE : return i8; 
            case INT : return i32; 
            case BOOL: return i1;
            case ARRAY: 
                switch(type_t.element_type->base_type){
                    case BYTE: return i8ptr;
                    case INT: return i32ptr;
                    default: return nullptr;
                }
            default : return nullptr;
        }
    }


void StmtBlock::sem(RetType &ret_t) {
        for (Stmt *s: stmt_list) {
            s->sem(ret_t);
            FunctionCall *sf = dynamic_cast<FunctionCall *>(s);
            if (sf!= nullptr && sf->get_type()!= AlanType(PROC))
                yyerror("Return value of non-void funtion '%s' is not used.\n", WARNING, num_line, sf->get_id().c_str());
        }
    }


