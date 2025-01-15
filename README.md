# Alan Compiler

## Dependencies 
```
llvm-config --version >= 19
clang --version >= 19
llc --version >= 19
```

The source code of the compiler was written based on llvm@19.1.3 and the updated handling of pointers. It assumes -opaque-pointers is the default behaviour for llc. 
(* In case of error add the following flag in cli/clang.cpp *) 

Follow the steps below to build and use the compiler.

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

#### Example Programs

You can try out some example programs included in the examples directory. Feel free to experiment and introduce errors to see how the compiler handles them.
```
./alanc -O ../examples/bubble_sort.alan
./alanc -O -d ../examples/factorial_ref.alan
```

Enjoy using the Alan compiler! 🎉
