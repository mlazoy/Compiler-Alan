# Alan Compiler

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
cat factorial_ref.alan.symbol
```

Enjoy using the Alan compiler! 🎉
