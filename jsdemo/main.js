
// Include a simple js file that is not compiled from OCaml
var foo = require("./external.js")

//Include code that is exported from OCaml. Note
//that all exported items will be in one object
var my = require("./code.js")

// Call multi-argument function 'boo' and get an integer 'hello'
console.log(foo(my.boo(3,my.hello),4))

//Get a string. Note how we need to do .c to get the actual JavaScript string
console.log(my.mystr.c)

//Just shows that we can print out the whole structure. Very good for testing.
console.log(my)

//When we pass a string to OCaml, we need to construct an object, where
//element 'c' contains the actual string, and 'l' is the length of the string.
console.log(my.strtest({c:'hello',l:5}).c)
