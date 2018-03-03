# JavaScriptish
JavaScriptish is a small and simple subset of the JavaScript language, together with a few additional extensions. JavaScriptish is particullarly designed for teaching beginner courses in fundamental programming. 


## Installation

1. Install Node
2. Install OCaml
3. Install the node package bs-platform

opam install js_of_ocaml


##  Getting Started

## Language Overview

## Some design decisions

* The language is designed to avoid some common beginner mistakes . Examples:

  - `while` loops and `if` statements needs to have scopes defined using `{` and `}`.
     It is not possible to have a single statement without defining a scope.
  - Assignments `=` is only allowed as a statement, and not within an expression.

### Missing constructs
The following JavaScript constructs are still missing

* Classes 
* Objects
* Numbers (floating point)
* Strings  
* Anonymous functions
  
TODOs:
- Add test script
- Add run
- Rename print to node and add description in help
- Check spelling  
  
### Extensions
Besides the pure subset of JavaScript described above, our plan is to extend Javascriptish with a few more language features. The main purpose of these extensions is to make it easy to demonastrate and teach functional language concepts. This is work in progress. Some of the concepts that are planned are the following:

* Tuples, as in functional languages such as OCaml and Haskell.
* Gradual typing (to be able to mix static and dynamic typing).
* Algebraic data types as in Haskell and OCaml.

## License
MIT License

Copyright (c) 2018 David Broman

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.


