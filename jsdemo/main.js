

var foo = require("./external.js")
var my = require("./code.js")


console.log(foo(my.boo(3,my.hello),4))
console.log(my.mystr.c)
console.log(my)
console.log(my.strtest("hello").c)
