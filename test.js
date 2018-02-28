

var foo = require("./ext1.js")
var my = require("./ext2.js")


console.log(foo(my.boo(3,my.hello),4))
console.log(my.mystr.c)

// To install:
// opam install js_of_ocaml

// To run:
// ocamlbuild -pkgs js_of_ocaml ext2.byte && js_of_ocaml ext2.byte && node test.js
