.PHONY: clean distclean default 

MIN_LLVM_VERSION := 19
LLVM_VERSION := $(shell llvm-config --version)

compare_versions = $(shell echo "$(1) $(2)" | awk '{if ($1 < $2) exit 1}')

# Check if the LLVM version is at least the required version
ifeq ($(call compare_versions, $(LLVM_VERSION), $(MIN_LLVM_VERSION)), 1)
    $(error "Error: llvm-config version $(LLVM_VERSION) is less than the required version $(MIN_LLVM_VERSION).")
endif

LLVMCONFIG=llvm-config

CXX=clang++
CXXFLAGS=-Wall -std=c++17 -g `$(LLVMCONFIG) --cxxflags`
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

# Ensure that the include directory is added
INCLUDES=`$(LLVMCONFIG) --includedir`
CXXFLAGS += -I$(INCLUDES)

default: alan

lexer.cpp: lexer.l 
	flex -s -o lexer.cpp lexer.l 

lexer.o: lexer.cpp lexer.hpp parser.hpp symbol.hpp error.hpp
	$(CXX) $(CXXFLAGS) -c lexer.cpp -o lexer.o

parser.cpp parser.hpp: parser.y
	bison -dv -o parser.cpp parser.y 

parser.o: parser.cpp lexer.hpp symbol.hpp error.hpp
	$(CXX) $(CXXFLAGS) -c parser.cpp -o parser.o

ast.o: ast.cpp ast.hpp
	$(CXX) $(CXXFLAGS) -c ast.cpp -o ast.o

libalan.o : libalan.cpp 
	$(CXX) $(CXXFLAGS) -c libalan.cpp -o libalan.o 

libalan.a : libalan.o 
	ar rcs libalan.a libalan.o 

alan: lexer.o parser.o ast.o libalan.a 
	$(CXX) $(CXXFLAGS) -o alan $^ $(LDFLAGS)

clean: 
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o 

distclean: clean 
	$(RM) alan
