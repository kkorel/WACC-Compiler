# WACC-Compiler

This is a compiler for WACC, a small imperative language used for the second-year
Compilers coursework at Imperial College London. It compiles WACC source files to
x86-64 assembly. We built this as a group project, working in a group of three.

## The WACC language

WACC is a small While-like language with:

- Base types `int`, `bool`, `char`, `string`, plus arrays and pairs
- `if`/`then`/`else`/`fi` and `while`/`do`/`done` control flow
- Functions with typed parameters and a mandatory `return`/`exit` on every path
- Pair operations (`newpair`, `fst`, `snd`, `null`)
- Built-in statements: `print`, `println`, `read`, `free`, `exit`, `skip`
- Unary operators `!`, `-`, `len`, `ord`, `chr`, and the usual arithmetic,
  comparison and boolean binary operators

The full grammar and semantics are defined by Imperial's WACC specification; this
repository is our implementation of a compiler for it.

## Pipeline

The compiler is written in Scala 3 and is structured as a standard multi-stage
pipeline, with each stage living in its own file under `src/main/wacc`:

1. **Lexing and parsing** (`lexer.scala`, `parser.scala`) build an AST
   (`ast.scala`) using the `parsley` parser combinator library. Every AST node
   carries its source position so later stages can report accurate error
   locations.
2. **Renaming** (`renamer.scala`) resolves scoping: every variable is renamed to
   a unique identifier, and out-of-scope references or illegal redeclarations
   are caught here.
3. **Type checking** (`typeChecker.scala`) checks the renamed AST against
   WACC's type rules, including unification for arrays and pairs, function
   signatures, and return-type consistency.
4. **Code generation** happens in two steps, under `backend/`:
   - `stackMachine.scala` lowers the typed AST into a generic stack-based IR
     (loads, stores, jumps, arithmetic).
   - `x86Lowerer.scala` lowers that IR into concrete x86-64 instructions
     (`x86IR.scala`), assigning each variable a fixed stack slot and inserting
     the runtime checks WACC requires (integer overflow, division by zero,
     null pair dereference, array bounds).
5. **Formatting** (`x86Formatter.scala`) prints the final x86-64 assembly
   (Intel syntax), including the read-only data section for string literals
   and the runtime support routines (printing, reading, error handling) that
   the generated code calls into via libc (`printf`, `scanf`, `malloc`,
   `free`, `exit`).

`Main.scala` wires these stages together: it reads a `.wacc` file, runs it
through parsing, renaming and type checking, and either reports a syntax error
(exit code 100) or semantic error (exit code 200), or emits a `.s` assembly
file next to the source on success.

Errors are formatted with the surrounding source lines and a caret pointing at
the offending token (`errors.scala`), rather than just a raw message.

## Repository layout

```
src/main/wacc/
  Main.scala          entry point, drives the compilation stages
  lexer.scala          tokeniser
  parser.scala          parser combinators, builds the AST
  ast.scala          AST node definitions
  renamer.scala          scope resolution / alpha-renaming
  typeChecker.scala          semantic type checking
  errors.scala          shared error types and formatting
  backend/
    stackMachine.scala          AST -> stack-based IR
    x86Lowerer.scala          stack IR -> x86-64 IR, runtime checks
    x86IR.scala          x86-64 instruction/operand definitions
    x86Formatter.scala          x86-64 IR -> assembly text
    stackCode.scala, backendConsts.scala          IR instruction set, shared constants
src/test/wacc/
  unit_test/          per-stage unit tests (parser, renamer, type checker, IR, lowering)
  integration_test/          frontend and backend integration tests
```

## Building and running

The project is built with `scala-cli` (see `project.scala` for the Scala
version and dependencies: `parsley` for parsing and `scalatest` for tests).

To compile a WACC source file during development:

```
scala run . -- path/to/file.wacc
```

This produces `file.s`, an x86-64 assembly file, which can be assembled and
linked with `gcc`.

`compile` is the front-end script expected by the coursework's test harness;
it runs the built `wacc-compiler` binary. `Makefile` builds a native image of
the compiler with GraalVM for use in CI/lab testing; it isn't meant to be used
for day-to-day development.

## Testing

Unit tests for each stage live under `src/test/wacc/unit_test` and can be run
with:

```
scala test .
```

The integration tests (`src/test/wacc/integration_test`) additionally compile
and, for the backend suite, assemble and run a set of official WACC example
programs, checking exit codes and (for the backend suite) program output
against the expected values recorded in each example file. These require
Imperial's WACC examples repository to be available locally (via the
`WACC_EXAMPLES` environment variable, defaulting to `wacc_examples`) and, for
the backend suite, a working `gcc` on the `GCC_PATH`. `.gitlab-ci.yml` shows
how these are wired together in CI, since this project was originally
developed on Imperial's internal GitLab.
