## Demo of js\_of\_ocaml

This folder includes a simple example of how to compile OCaml code
into JavaScript, and then to include selected functions into a `.js`
file, which can be executed using node.js

Files:

* `Makefile` A simple make file that shows the commands for compiling and executing a program.
* `code.ml` The example OCaml code that exports a function, an integer, and a string.
* `external.js` A simple JavaScript file that is imported.
* `main.js` The main JavaScript file that imports both code from `code.ml` and `external.js`.

Before running this program, you need to have installed `node.js` and `opam`, where the latter is a package handler for OCaml.

If you have `opam` installed, you need to install the following packages using command:

`>> opam install ocamlbuild js-of-ocaml`

To be able to use JavaScript in the browser, you also need to install
the browserify npm module:

`npm install -g browserify`

(Note: the example code support for the web-browser is still not completely working)

Finally, if you write

`>> make`

it should print out number 60 and some more text.

The makefile includes the following three commands:

	ocamlbuild -pkgs js_of_ocaml code.byte
	js_of_ocaml code.byte
	node main.js

The first line compiles the file `code.ml` into byte code. Note that
the package `js_of_ocaml` needs to be included. The second line
compiles the byte code into Java script. Finally, the last line
executes `main.js`, which imports and calls the OCaml code from
JavaScript.
