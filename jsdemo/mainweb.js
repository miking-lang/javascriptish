

// This is the same code as main.js, with the difference
// that console.log() is replaced with document.write()
// That is, this file is used in the brower, whereas main.js
// is used for node.js

// NOTE: This does not yet work.

var foo = require("./external.js")
var my = require("./code.js")b
document.write(foo(my.boo(3,my.hello),4))
document.write(my.mystr.c)
document.write(my)
document.write(my.strtest({c:'hello',l:5}).c)
