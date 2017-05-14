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
- pattern matching on variatn types (only in the `case` statement, 
  but not in function declarations),
- recursive inc. mutually recursive functions, data types and values 
  (infinite lists as in Haskell),
- language constructs like `let`, `where`, anonymous functions (one parameter only),
  named functions (support multiple parameters,
  but no pattern matching in declarations),
- case of the first letter in the identifier is distinguished.

Distinct features:
- IO () and String are not supported, so main is just meant to be any expression. 
  It will be evaluated and the value will be printed.

The BNFC compatible grammar is available in `src/grammar.cf`.
Examples are available in the `good` directory.

Using the interpreter
---------------------

To build:

  stack build

or:
  
  make

To run:

  stack exec interpreteur-exe filename.hs

or:

  ./interpreter

How to run on students
----------------------

Running on students requires configuring stack first. Run the following commands
in the project dir.

  export PATH=/home/students/inf/PUBLIC/MRJP/Stack/:$PATH
  stack setup
  stack build

Done.

What has already been implemented
---------------------------------

Python-like interpreter - no type enforcement, but all features listed above.

What is yet to be implemented
-----------------------------

- "make" and "./interpreter" commands as shortcuts to bnfc commands.
- Type declarations in the grammar.
- Static type checking including type inference.
- Friendlier error reporting (e.g. line number)
- "standard library" including List, Maybe and basic functions
- `bad` examples
