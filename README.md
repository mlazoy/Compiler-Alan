# Alan Compiler

This repository contains the Alan compiler and its CLI tool. Follow the steps below to build and use the compiler.

### Building the Alan Compiler

Navigate to the 'compiler' directory and build:
```
cd compiler
make
```

### Building the CLI

Navigate to the 'cli' directory and build:
```
cd cli
make
```

### Using the Compiler

To use the compiler, type the following command inside the cli directory:
```
./alanc [Flags] [Program]
```

#### Example Programs

You can try out some example programs included in the examples directory. Feel free to experiment and introduce errors to see how the compiler handles them.
```
./alanc -O ../examples/hanoi.alan
./alanc -O -d ../examples/factorial.alan
```

Enjoy using the Alan compiler! 🎉
