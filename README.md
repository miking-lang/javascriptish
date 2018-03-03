# JavaScriptish

JavaScriptish is a small and simple superset of a pure subset of the JavaScript language. JavaScriptish is
particularly designed for teaching beginner courses in fundamental
programming.

### Contributors

 * [David Broman](https://people.kth.se/~dbro/), KTH Royal Institute of Technology, Sweden

## Installation

1. Install [Node](https://nodejs.org/en/)
2. Install [OCaml and OPAM](https://ocaml.org/docs/install.html)
3. Install js\_of\_ocaml using OPAM, by executing command `>>opam install js_of_ocaml`.
4. Clone or fork the [JavaScriptish GitHub repo](https://github.com/miking-lang/javascriptish)
5. Run `make` in the project root folder.


##  Getting Started

Go to directory `test`. This folder contains a number of small JavaScriptish files (with ending `.jsh`). If you are in directory `test` and execute the command

`jsh fact-imp.jsh`

file `fact-imp.jsh` is executed. The `.jsh` file File `fact-imp.jsh` contains the following code:

```javascript
function fact(n){
  var res=1
  while(n>1){
    res = res * n
    n = n - 1
  }
  return res
}

print(fact(5))
```

The example contains a pure subset of normal JavaScript, with the
exception for printing. In JavaScriptish, a call to `print` outputs
text to the standard output.


## Language Overview

### Missing constructs
The following JavaScript constructs are still missing

* Classes
* Objects
* Numbers (floating point)
* Strings
* Anonymous functions


### Some design decisions

The language is designed to avoid some common beginner mistakes. For instance:

* `while` loops and `if` statements always need to have scopes defined using `{` and `}`.
     It is not possible to have a single statement without defining a scope.

*  Assignments `=` is only allowed in a statement, and not within an expression.


### Extensions

Besides the pure subset of JavaScript described above, our plan is to
extend JavaScriptish with a few more language features. The main
purpose of these extensions is to make it easy to
teach functional language concepts. This is work in progress. Some of
the concepts that are planned are the following:

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
