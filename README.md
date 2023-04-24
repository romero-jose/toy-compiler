# Toy Compiler

A compiler for a simple functional programming language to LLVM IR.

## Usage

### Basic usage

To compile a program to LLVM IR, use the following command:

```sh
toy_compiler [-v] <input_file> [-o <output_file>]
```

* `input_file` is the program you want to compile

* `output_file` is where the result will be stored (default: input file with `.ll` extension)

Use `-v` to print the intermediate representation at every step of the compilation.

### Running the output

To run the compiled output you have two options:

1. Interpret the result with the LLVM interpreter:

```sh
lli <input_file>
```

2. Further compile the result into a native executable using Clang:

```sh
clang <input_file> -o <output_file>
```

## Installation

To install this project, first you need to install its dependencies using opam:

```sh
opam install --deps-only .
```

> Note: The project depends on LLVM 14 as a system dependency, so depending on your OS you may need to install it using your package manager.

Then, build the project using dune:

```sh
dune build
```

## Overview

### Compilation pipeline

The compilation pipeline consists of the following phases:

1. Lexing
2. Parsing
3. α-renaming
3. ANF conversion
4. β-reduction
5. Unused let removal
7. Closure conversion
8. LLVM IR code generation

### Intermediate Representation

A-Normal Form (ANF) is used as the IR, defined in `syntax/anf.ml`.

### Closure conversion

During closure conversion, the compiler extracts closures into top-level functions with a list of free variables. Additionally, it inserts explicit closure constructors in the positions where lambdas were defined.

Before:

```ml
let addn = fun n -> fun m -> n + m in
addn 1
```

After:

```ml
let fun lambda_1 m [n] = 
    n + m
in
let fun lambda_2 n [] =
    (closure lambda_1 n)
in
let addn = (closure lambda_2) in
(addn 1)
```

### Closures

During the code generation phase, closures are compiled into heap-stored arrays composed of a function pointer followed by the captured variables:

```
+-------+-------+-------+-------+
| f_ptr | var1  | ...   | var_n |
+-------+-------+-------+-------+
```

Functions always take a pointer to the closure as the first argument.
