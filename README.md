# Toy Compiler

A compiler for a simple functional programming language to LLVM IR.

## Usage

### Basic usage

To compile a program to LLVM IR you can use the following command:

```sh
toy_compiler [-v] <input_file> [-o <output_file>]
```

Additionally, you can specify the `-v` option to print the intermediate representation in every step of the compilation.

## Installation

To install this project first you need to install its dependencies using opam.

```sh
opam install --deps-only .
```

Afterwards, you can build the project using dune.

```sh
dune build
```

## Overview

### Compilation pipeline

Compilation follows the following phases:

1. Lexing
2. Parsing
3. α-renaming
3. ANF conversion
4. β-reduction
5. Unused let removal
7. Closure conversion
8. LLVM IR code generation

### Intermediate Representation

A-Normal Form (ANF) is used as the IR. It is defined in `syntax/anf.ml`.

### Closure conversion

The closure conversion phase extracts closures into top-level functions with a list of free variables. Additionally, it inserts explicit closure constructors in the positions where lambdas were defined.

```ml
(* before closure conversion *)
let addn = fun n -> fun m -> n + m in
addn 1
```

```ml
(* after closure conversion *)
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

In the code generation phase closures are compiled into heap stored arrays composed of a function pointer followed by the captured variables.

```
+-------+-------+-------+-------+
| f_ptr | var1  | ...   | var_n |
+-------+-------+-------+-------+
```

Functions always take a pointer to the closure as first argument.
