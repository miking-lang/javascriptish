# javascriptish
A simple superset of a java script subset. The language is designed for teaching.


## Installation

1. Install Node
2. Install OCaml
3. Install the node package bs-platform

opam install js_of_ocaml



## Some desgin decisions

* The language is designed to avoid some common beginner mistakes . Examples:

  - `while` loops and `if` statements needs to have scopes defined using `{` and `}`.
     It is not possible to have a single statement without defining a scope.
  - Assignments `=` is only allowed as a statement, and not within an expression.