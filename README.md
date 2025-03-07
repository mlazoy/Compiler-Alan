# Alan Compiler
![Lines of Code](https://img.shields.io/tokei/lines/github/mlazoy/Compiler-Alan)
![Languages](https://img.shields.io/github/languages/count/mlazoy/Compiler-Alan)
![Top Language](https://img.shields.io/github/languages/top/mlazoy/Compiler-Alan)
![Repo Size](https://img.shields.io/badge/repo--size-123KB-blue)


The official documentation of the Alan language for the needs of the project can be found [here](alan2024.pdf).

### Dependencies 
```
llvm-config --version >= 19
clang --version >= 19
llc --version >= 19
bison 2.3
flex 2.6.4
```

The source code of the compiler was written based on llvm@19.1.3 and the updated handling of pointers. It assumes -opaque-pointers is the default behaviour for llc (>=15).

*1. In case of llc error add the --opaque flag in the final cli/alanc executable as described below.* 

*2. If testing from Mac the -no-pie option during linking with clang is not needed. To silence this warning add the --pie flag in the final /cli/alanc executable.*

### Building the Alan Compiler

Download the repo and navigate to the root dorectory:
```
git clone https://github.com/mlazoy/Compiler-Alan.git
cd Compiler-Alan
```
Navigate to the 'compiler' directory and build:
```
cd compiler && make distclean; make
```

### Building the CLI

Navigate to the 'cli' directory and build:
```
cd cli && make distclean; make
```

### Usage

The compilation command is of the following format: 
```
./alanc [Flags] [Program]
```
Explore all supported options by typing :
```
./alanc --help
```
The final executable is saved as */path/to/your/program.out*. Intermediate and final code are saved as */path/to/your/program.imm* and */path/to/your/program.asm* respectively.

### Example Programs

You can try out some example programs included in the examples directory. Feel free to experiment and introduce errors to see how the compiler handles them.
```
./alanc -O ../examples/bubble_sort.alan
../examples/bubble_sort.out

./alanc -O -d ../examples/factorial_ref.alan
../examples/factorial_ref.out
cat factorial_ref.symbol
```

Enjoy using the Alan compiler! 🎉
