Restricted Haskell interpreter
==============================

Author: Michał Łuszczyk, ml360314

This interpreter is implemented in Haskell and supports a subset
of Haskell features.

Language
--------

Similarities to Haskell:

- static and strong typing with type inference,
- grammar inspired by Haskell,
- laziness,
- static binding,
- polymorphic variant types (including boolean), integers, functions,
- pattern matching on variant types (only in the `case` statement, 
  but not in function declarations),
- recursive inc. mutually recursive functions, data types and values 
  (infinite lists as in Haskell), this only works in the `-d` mode currently,
- language constructs like `let`, `where`, anonymous functions (one parameter only),
  named functions (support multiple parameters,
  but no pattern matching in declarations),
- case of the first letter in the identifier is distinguished.

Distinct features:
- IO () and String are not supported, so main is just meant to be any expression. 
  It will be evaluated and the value will be printed.

The BNFC compatible grammar is available in `src/grammar.cf`.

Examples can be found in the following directories:
- `good/unit` - simple examples, usually for a single feature of the interpreter,
- `good/complex` - examples using multiple features,
- `bad` - examples of code that doesn't work.

Using the interpreter
---------------------

To build:

  stack build

or:
  
  make

To run:

  stack exec interpreteur-exe filename.hs

or:

  ./interpreter filename.hs

Additional flags are available: flag `-t` disables interpreter and prints the
type of the `main` functoin. Flag `-d` is `dynamic mode` - the type checker is
not run, only the interpreter.

How to run on `students`
----------------------

Running on `students` requires configuring stack first. Run the following
commands in the project dir (this may and probably will take a long time).

  export PATH=/home/students/inf/PUBLIC/MRJP/Stack/:$PATH
  stack setup
  stack build

Done.

What has been implemented
-------------------------

The package consists of two de facto separate features - static type checking and
dynamic interpreter that does not enforce types (both are enabled by default).
Both parts are implemented, however static type checking misses some important 
features, so running with option `-d` (dynamic) will give the programmer more
flexibility.

Unimplemented extensions, potentially yet to be done
----------------------------------------------------

- Recursion in type checking.
- Checking whether type and function definitions are not repeated and whether
  the references to polymorphic variant types have correct number of parameters.
- Respecting type declarations/type hints like `a :: Int`.
- Checking whether all cases are matched in `case` expression.
- More sophisticated "builtins.hs" library including List, Maybe and basic Prelude
  functions.
- Syntax sugar for lists.
