

(* A simple function that multiplies integers *)
let boo a b = a * b

(* Some string *)
let mystr = "Number " ^ string_of_int 10 ^ " is a number."

(* Export of a function that takes a string as input, and returns another string *)
let strtest x = "[" ^ x ^ "]"

(* Exports all values. Note how an object is created on the JavaScript side *)
let _ = Js.export "boo" boo
let _ = Js.export "hello" 10
let _ = Js.export "mystr" mystr
let _ = Js.export "strtest" strtest
